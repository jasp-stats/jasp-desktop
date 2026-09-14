#include "browsertokenprovider.h"

#include "gui/aiconfigmodel.h"
#include "log.h"

#include <QAbstractOAuth2>
#include <QDesktopServices>
#include <QHostAddress>
#include <QJsonDocument>
#include <QJsonObject>
#include <QOAuth2AuthorizationCodeFlow>
#include <QOAuthHttpServerReplyHandler>
#include <QSet>
#include <QTimer>
#include <QUrl>

#include <chrono>

// =============================================================================
// Helpers
// =============================================================================

/// Turn whatever the config holds — a tenant id, "organizations", an authority
/// URL, or a full issuer URL — into the authority the v2 endpoints hang off.
static QString authorityBase(const QString &authority)
{
	QString base = authority.trimmed();

	if (base.isEmpty())
		base = QStringLiteral("https://login.microsoftonline.com/organizations");
	else if (!base.startsWith(QStringLiteral("http://"), Qt::CaseInsensitive)
	      && !base.startsWith(QStringLiteral("https://"), Qt::CaseInsensitive))
		base.prepend(QStringLiteral("https://login.microsoftonline.com/"));

	while (base.endsWith(QLatin1Char('/')))
		base.chop(1);

	// An issuer URL ends in /v2.0; the endpoints hang off the authority above it.
	if (base.endsWith(QStringLiteral("/v2.0"), Qt::CaseInsensitive))
		base.chop(5);

	return base;
}

static QString requestFailedReason(QAbstractOAuth::Error error)
{
	switch (error)
	{
	case QAbstractOAuth::Error::NoError:
		return QStringLiteral("Sign-in failed.");
	case QAbstractOAuth::Error::NetworkError:
		return QStringLiteral("Could not reach the sign-in service. Check your network connection and try again.");
	case QAbstractOAuth::Error::ServerError:
		return QStringLiteral("The sign-in service returned an error. Please try again.");
	case QAbstractOAuth::Error::OAuthTokenNotFoundError:
		return QStringLiteral("The sign-in service did not return a token. Check that the configured scope is allowed for this application.");
	case QAbstractOAuth::Error::OAuthTokenSecretNotFoundError:
		return QStringLiteral("The sign-in service did not return a token secret.");
	case QAbstractOAuth::Error::OAuthCallbackNotVerified:
		return QStringLiteral("The sign-in redirect could not be verified. Check that the application registration allows http://localhost.");
	case QAbstractOAuth::Error::ClientError:
		return QStringLiteral("The sign-in request was rejected. Check the authority and client ID.");
	case QAbstractOAuth::Error::ExpiredError:
		return QStringLiteral("The sign-in request expired. Please try again.");
	}
	return QStringLiteral("Sign-in failed.");
}

/// Read a display name out of the id_token. The token was just received over TLS
/// directly from the token endpoint, so the payload is read without verifying the
/// signature — this is for display only, never for an access decision.
static QString accountFromIdToken(const QString &idToken)
{
	const QList<QByteArray> parts = idToken.toLatin1().split('.');
	if (parts.size() < 2)
		return {};

	const QByteArray payload = QByteArray::fromBase64(
		parts.at(1), QByteArray::Base64UrlEncoding | QByteArray::OmitTrailingEquals);
	if (payload.isEmpty())
		return {};

	const QJsonObject claims = QJsonDocument::fromJson(payload).object();
	const char *keys[] = { "preferred_username", "upn", "email", "name" };
	for (const char *key : keys)
	{
		const QString value = claims.value(QLatin1String(key)).toString();
		if (!value.isEmpty())
			return value;
	}
	return {};
}

// =============================================================================
// BrowserTokenProvider
// =============================================================================

BrowserTokenProvider::BrowserTokenProvider(QObject *parent)
	: TokenProvider(parent)
	, m_flow(new QOAuth2AuthorizationCodeFlow(this))
	, m_timeout(new QTimer(this))
{
	// The user may have wandered off to the browser; don't hold the listener open
	// forever.
	m_timeout->setSingleShot(true);
	m_timeout->setInterval(5 * 60 * 1000);
	connect(m_timeout, &QTimer::timeout, this, [this]() {
		fail(QStringLiteral("Sign-in timed out. Please try again."));
	});

	// One listener for the lifetime of the provider: it is closed between attempts
	// and re-listened (which picks a fresh ephemeral port) rather than recreated,
	// so the flow never ends up holding a pointer to a destroyed handler.
	m_replyHandler = new QOAuthHttpServerReplyHandler(quint16(0), this);

	// Qt's default handler advertises itself as 127.0.0.1, but the app
	// registration — and the portal text box — only accepts http://localhost.
	// Without this the redirect fails to match and Entra returns AADSTS50011.
	m_replyHandler->setCallbackHost(QStringLiteral("localhost"));
	m_replyHandler->setCallbackText(QStringLiteral(
		"<html><head><title>JASP</title></head>"
		"<body style=\"font-family:sans-serif;padding:2rem\">"
		"<h3>You are signed in</h3>"
		"<p>You can close this tab and return to JASP.</p>"
		"</body></html>"));
	m_flow->setReplyHandler(m_replyHandler);

	// S256 is Qt's default from 6.8, but state it: this is what makes the flow
	// safe for a public client, where there is no client secret to protect the
	// authorization code.
	m_flow->setPkceMethod(QOAuth2AuthorizationCodeFlow::PkceMethod::S256);

	// Renew ahead of expiry so a long chat session doesn't stall on a dead token.
	// Qt reports the new token through tokenChanged(), which is all we listen for.
	m_flow->setAutoRefresh(true);
	m_flow->setRefreshLeadTime(std::chrono::minutes(5));

	// Qt only *emits* authorizeWithBrowser — nothing opens the browser unless we
	// do. Always the system browser, never an embedded view: Conditional Access
	// device-compliance fails inside embedded web views.
	connect(m_flow, &QAbstractOAuth::authorizeWithBrowser, this, [](const QUrl &url) {
		QDesktopServices::openUrl(url);
	});

	connect(m_flow, &QAbstractOAuth::granted,      this, &BrowserTokenProvider::onGranted);
	connect(m_flow, &QAbstractOAuth::tokenChanged, this, &BrowserTokenProvider::onTokenChanged);

	connect(m_flow, &QAbstractOAuth::requestFailed, this, [this](QAbstractOAuth::Error error) {
		onRequestFailed(requestFailedReason(error));
	});

	// Entra explains exactly what is wrong (bad scope, redirect URI not
	// registered, consent required). Prefer that over a generic message.
	connect(m_flow, &QAbstractOAuth2::serverReportedErrorOccurred, this,
			[this](const QString &error, const QString &description, const QUrl &) {
		onServerError(error, description);
	});

	// A refresh the provider rejects leaves us NotAuthenticated with no access
	// token. That's our cue to ask the user rather than give up.
	connect(m_flow, &QAbstractOAuth::statusChanged, this, [this](QAbstractOAuth::Status status) {
		if (status == QAbstractOAuth::Status::NotAuthenticated && m_refreshing)
			beginSignIn();
	});
}

BrowserTokenProvider::~BrowserTokenProvider()
{
	cancelAttempt();
}

const QString &BrowserTokenProvider::defaultClientId()
{
	// "JASP AI Desktop" — the publisher-verified multi-tenant registration every
	// JASP build shares (plan §1b). A customer with its own registration sets
	// authClientId, and then this is not used.
	static const QString id = QStringLiteral("fc57bc92-9de6-405e-8a47-4161cc3e27d3");
	return id;
}

QString BrowserTokenProvider::authMode() const
{
	return QStringLiteral("oidc");
}

QString BrowserTokenProvider::token() const
{
	return m_flow ? m_flow->token() : QString();
}

QDateTime BrowserTokenProvider::expiresAt() const
{
	return m_flow ? m_flow->expirationAt() : QDateTime();
}

QString BrowserTokenProvider::accountName() const
{
	return m_account;
}

bool BrowserTokenProvider::isValid() const
{
	if (!m_flow || m_flow->token().isEmpty())
		return false;

	const QDateTime expiry = m_flow->expirationAt();
	if (!expiry.isValid())
		return true;   // the provider didn't say; assume it is usable

	// Treat "about to expire" as invalid so the caller renews first rather than
	// sending a token that dies mid-request. QDateTime comparison handles the
	// mixed time specs (Qt builds this one from local time).
	return expiry > QDateTime::currentDateTimeUtc().addSecs(60);
}

bool BrowserTokenProvider::configure(QString *error)
{
	AIConfigModel *cfg = AIConfigModel::config();
	if (!cfg)
	{
		*error = QStringLiteral("AI configuration is not available.");
		return false;
	}

	const QString authority = cfg->currentAuthAuthority();
	const QString scope     = cfg->currentAuthScope();
	QString clientId        = cfg->currentAuthClientId();
	if (clientId.isEmpty())
		clientId = defaultClientId();

	if (scope.isEmpty())
	{
		*error = QStringLiteral("No scope is configured for this provider, so there is nothing to request a token for. "
								"For Azure OpenAI the scope is https://cognitiveservices.azure.com/.default.");
		return false;
	}

	// Different provider, or the same one edited: anything cached belongs to the
	// old configuration and must not be sent to the new endpoint.
	const QString signature = authority + QLatin1Char('|') + scope + QLatin1Char('|') + clientId;
	if (signature != m_configSignature)
	{
		signOut();
		m_configSignature = signature;
	}

	const QString base = authorityBase(authority);
	m_flow->setClientIdentifier(clientId);
	m_flow->setAuthorizationUrl(QUrl(base + QStringLiteral("/oauth2/v2.0/authorize")));
	m_flow->setTokenUrl(QUrl(base + QStringLiteral("/oauth2/v2.0/token")));

	// openid   → an id_token, which is where the account name comes from
	// profile  → the display name inside that id_token
	// offline_access → a refresh token, without which every expiry starts over
	QSet<QByteArray> scopes{
		QByteArrayLiteral("openid"),
		QByteArrayLiteral("profile"),
		QByteArrayLiteral("offline_access"),
	};
	scopes.insert(scope.toUtf8());
	m_flow->setRequestedScopeTokens(scopes);

	return true;
}

void BrowserTokenProvider::ensureToken()
{
	if (isValid())
	{
		emit tokenReady(m_flow->token());
		return;
	}

	// Already waiting on the user — don't open a second browser tab.
	if (m_running)
		return;

	QString error;
	if (!configure(&error))
	{
		fail(error);
		return;
	}

	// With a refresh token we can often renew without the user. Qt drives that
	// through the same signals as grant(); if the provider rejects it we end up
	// NotAuthenticated, which beginSignIn() below picks up.
	if (!m_flow->refreshToken().isEmpty())
	{
		Log::log() << "BrowserTokenProvider: attempting silent token refresh" << std::endl;
		m_refreshing = true;
		m_running    = true;
		m_timeout->start();
		m_flow->refreshTokens();
		return;
	}

	beginSignIn();
}

void BrowserTokenProvider::beginSignIn()
{
	m_refreshing = false;

	// Bind loopback explicitly. Qt's loopback fallback only triggers for a *null*
	// address, but listen()'s default is a non-null one — so a no-argument call
	// binds 0.0.0.0, which exposes this redirect endpoint to the local network and
	// triggers a Windows Firewall prompt for a listener that only ever needs the
	// browser on this machine. The port is ephemeral, and the registration matches
	// http://localhost regardless of port (plan Appendix A).
	m_replyHandler->close();

	if (!m_replyHandler->listen(QHostAddress::LocalHost, 0))
	{
		fail(QStringLiteral("Could not open a local port to receive the sign-in redirect. "
							"JASP needs to listen on localhost to complete sign-in."));
		return;
	}

	Log::log() << "BrowserTokenProvider: sign-in redirect URI is "
			   << m_replyHandler->callback().toStdString() << std::endl;

	m_running = true;
	m_timeout->start();
	m_flow->grant();
}

void BrowserTokenProvider::cancelAttempt()
{
	m_running    = false;
	m_refreshing = false;

	if (m_timeout)
		m_timeout->stop();

	// Closed but not destroyed: the flow holds this pointer, and the next attempt
	// simply re-listens on it.
	if (m_replyHandler)
		m_replyHandler->close();
}

void BrowserTokenProvider::onGranted()
{
	// The browser is done with us; stop listening. The token itself arrives
	// through onTokenChanged().
	cancelAttempt();
}

void BrowserTokenProvider::onTokenChanged(const QString &accessToken)
{
	if (accessToken.isEmpty())
		return;

	m_account = accountFromIdToken(m_flow->idToken());

	// Covers a silent refresh, which may not emit granted().
	cancelAttempt();

	Log::log() << "BrowserTokenProvider: token acquired"
			   << (m_account.isEmpty() ? std::string() : " for " + m_account.toStdString())
			   << std::endl;

	emit tokenReady(accessToken);
}

void BrowserTokenProvider::onServerError(const QString &error, const QString &description)
{
	const QString message = description.isEmpty()
		? error
		: error + QStringLiteral(": ") + description;
	fail(message.trimmed());
}

void BrowserTokenProvider::onRequestFailed(const QString &reason)
{
	if (m_refreshing)
	{
		// The refresh never got off the ground; ask the user instead.
		Log::log() << "BrowserTokenProvider: silent refresh failed (" << reason.toStdString()
				   << "), falling back to interactive sign-in" << std::endl;
		beginSignIn();
		return;
	}

	fail(reason);
}

void BrowserTokenProvider::fail(const QString &message)
{
	cancelAttempt();

	Log::log() << "BrowserTokenProvider: " << message.toStdString() << std::endl;
	emit authFailed(message);
}

void BrowserTokenProvider::signOut()
{
	cancelAttempt();

	if (m_flow)
	{
		m_flow->setToken(QString());
		m_flow->setRefreshToken(QString());
	}

	m_account.clear();
}
