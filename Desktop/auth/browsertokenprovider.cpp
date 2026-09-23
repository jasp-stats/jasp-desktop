#include "browsertokenprovider.h"

#include "auth/secretvault.h"
#include "log.h"

#include <QAbstractOAuth2>
#include <QDesktopServices>
#include <QHostAddress>
#include <QJsonDocument>
#include <QJsonObject>
#include <QOAuth2AuthorizationCodeFlow>
#include <QOAuthHttpServerReplyHandler>
#include <QSet>
#include <QStringList>
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

/// Identity providers return these percent-encoded, and Qt passes them through
/// untouched — which reads as "AADSTS650057%3A+Invalid+resource" in a message
/// shown to a user.
static QString decodeUrlComponent(const QString &text)
{
	QByteArray bytes = text.toUtf8();
	bytes.replace('+', ' ');
	return QUrl::fromPercentEncoding(bytes);
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
		return QStringLiteral("The sign-in redirect could not be verified. Check that the application registration allows http://127.0.0.1/callback.");
	case QAbstractOAuth::Error::ClientError:
		return QStringLiteral("The sign-in request was rejected. Check the authority and client ID.");
	case QAbstractOAuth::Error::ExpiredError:
		return QStringLiteral("The sign-in request expired. Please try again.");
	}
	return QStringLiteral("Sign-in failed.");
}

/// The JWT payload, or an empty object if the token is not a JWT. Entra access
/// and ID tokens are JWTs; another provider may return an opaque token.
///
/// The token was just received over TLS straight from the token endpoint, so the
/// payload is read without verifying the signature — this is for display and
/// diagnostics only, never for an access decision.
static QJsonObject jwtClaims(const QString &token)
{
	const QList<QByteArray> parts = token.toLatin1().split('.');
	if (parts.size() < 2)
		return {};

	const QByteArray payload = QByteArray::fromBase64(
		parts.at(1), QByteArray::Base64UrlEncoding | QByteArray::OmitTrailingEquals);
	if (payload.isEmpty())
		return {};

	return QJsonDocument::fromJson(payload).object();
}

/// A display name out of the id_token, for the UI.
static QString accountFromIdToken(const QString &idToken)
{
	const QJsonObject claims = jwtClaims(idToken);
	const char *keys[] = { "preferred_username", "upn", "email", "name" };
	for (const char *key : keys)
	{
		const QString value = claims.value(QLatin1String(key)).toString();
		if (!value.isEmpty())
			return value;
	}
	return {};
}

/// What the provider persists in the vault: the refresh token, plus the account
/// name so the UI can show who is signed in before any token exists. The access
/// token is deliberately not stored — it is a JWT that can exceed the vault's
/// blob limit, and a fresh one costs a single call to the token endpoint.
static QByteArray encodeStoredToken(const QString &refreshToken, const QString &account)
{
	QJsonObject o;
	o[QStringLiteral("refreshToken")] = refreshToken;
	o[QStringLiteral("account")]      = account;
	return QJsonDocument(o).toJson(QJsonDocument::Compact);
}

/// False when the blob is not one of ours, or carries no refresh token.
static bool decodeStoredToken(const QByteArray &blob, QString *refreshToken, QString *account)
{
	const QJsonObject o = QJsonDocument::fromJson(blob).object();
	*refreshToken = o.value(QStringLiteral("refreshToken")).toString();
	*account      = o.value(QStringLiteral("account")).toString();
	return !refreshToken->isEmpty();
}

/// A short, non-secret summary of what the access token is good for — its
/// audience and granted scopes. Logged on success so a working sign-in can be
/// confirmed even when there is no resource to call yet. Never logs the token.
static QString describeToken(const QString &accessToken)
{
	const QJsonObject claims = jwtClaims(accessToken);
	if (claims.isEmpty())
		return QStringLiteral("opaque token");

	QStringList parts;
	const QString audience = claims.value(QStringLiteral("aud")).toString();
	if (!audience.isEmpty())
		parts << QStringLiteral("aud=") + audience;

	// Entra puts delegated scopes in "scp" and application roles in "roles".
	for (const char *key : { "scp", "roles" })
	{
		const QString value = claims.value(QLatin1String(key)).toString();
		if (!value.isEmpty())
			parts << QString::fromLatin1(key) + QLatin1Char('=') + value;
	}

	// Identity claims. These are what the resource matches a role assignment
	// against, so when a call comes back 401/PermissionDenied the first question
	// is "which principal, in which tenant, did we actually send?" Answer it here
	// instead of inferring it from whichever account the browser happened to use.
	// "oid" is the user object ID shown in the portal's role-assignment list.
	for (const char *key : { "oid", "tid" })
	{
		const QString value = claims.value(QLatin1String(key)).toString();
		if (!value.isEmpty())
			parts << QString::fromLatin1(key) + QLatin1Char('=') + value;
	}

	return parts.isEmpty() ? QStringLiteral("no aud/scp claims") : parts.join(QStringLiteral(" "));
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

	// The redirect mirrors Claude Desktop's convention — http://127.0.0.1/callback
	// — which is what the app registration now carries, and what a customer
	// following Anthropic's gateway guide registers for a bring-your-own app.
	// The host is Qt's default, stated anyway: Entra matches redirect URIs
	// exactly (host and path; only the port is free for loopback), so a mismatch
	// here fails with AADSTS50011.
	m_replyHandler->setCallbackHost(QStringLiteral("127.0.0.1"));
	m_replyHandler->setCallbackPath(QStringLiteral("/callback"));
	// Qt serves this for *any* callback at this path — success, an error redirect,
	// or garbage — always HTTP 200, always this text, without inspecting the
	// result. It also does so before the code is exchanged for a token, so it must
	// not claim success. Real failures are reported in JASP. Qt wraps this in its
	// own html/body, so pass a fragment.
	m_replyHandler->setCallbackText(QStringLiteral(
		"<div style=\"font-family:sans-serif;padding:2rem\">"
		"<h3>Sign-in response received</h3>"
		"<p>You can close this tab and return to JASP.</p>"
		"</div>"));
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
		if (m_refreshing)
			refreshFailed(requestFailedReason(error), error == QAbstractOAuth::Error::NetworkError);
		else
			onRequestFailed(requestFailedReason(error));
	});

	// Entra explains exactly what is wrong (bad scope, redirect URI not
	// registered, consent required). Prefer that over a generic message.
	// A server answer during a silent renewal means the stored refresh token
	// was rejected — typically revoked — which retrying the same renewal cannot
	// fix, so route it to refreshFailed() instead.
	connect(m_flow, &QAbstractOAuth2::serverReportedErrorOccurred, this,
			[this](const QString &error, const QString &description, const QUrl &) {
			if (m_refreshing)
			{
				refreshFailed(decodeUrlComponent(
					description.isEmpty() ? error : error + QStringLiteral(": ") + description), false);
				return;
			}
			onServerError(error, description);
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

void BrowserTokenProvider::setOidcConfig(const OidcConfig &config)
{
	QString clientId = config.clientId;
	if (clientId.isEmpty())
		clientId = defaultClientId();

	// The port is not part of the signature (it cannot affect tokens), so apply
	// it before the early return — a port-only edit must still take effect.
	m_redirectPort = (config.redirectPort > 0 && config.redirectPort <= 65535)
						 ? config.redirectPort : 0;

	// A different provider, or the same one edited: anything cached belongs to
	// the old configuration and must not be sent to the new endpoint.
	const QString signature =
		config.authority + QLatin1Char('|') + config.scope + QLatin1Char('|') + clientId;
	if (signature == m_configSignature)
		return;

	signOut();
	m_configSignature = signature;
	m_authority       = config.authority;
	m_scope           = config.scope;
	m_clientId        = clientId;

	// The vault may hold a usable token for the new configuration.
	m_vaultTried = false;
}

bool BrowserTokenProvider::configure(QString *error)
{
	if (m_scope.isEmpty())
	{
		*error = QStringLiteral("No scope is configured for this provider, so there is nothing to request a token for. "
								"For Azure OpenAI the scope is https://cognitiveservices.azure.com/.default.");
		return false;
	}

	const QString base = authorityBase(m_authority);
	m_flow->setClientIdentifier(m_clientId);
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
	scopes.insert(m_scope.toUtf8());
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

	// Already waiting on the user — or on a silent renewal from the vault —
	// don't open a second browser tab on top of it. The in-flight attempt ends
	// in tokenReady(), which flushes whatever request parked behind it.
	if (m_running || m_refreshing)
		return;

	QString error;
	if (!configure(&error))
	{
		fail(error);
		return;
	}

	// Renewal inside a session is Qt's job: autoRefresh() + refreshLeadTime()
	// replace the access token ahead of expiry, and tokenChanged() picks the new
	// one up. So reaching this point means nothing usable is in memory. Before
	// opening a browser, try a refresh token persisted by a previous run: it
	// renews silently and is invisible to the user.
	if (!m_vaultTried)
	{
		m_vaultTried = true;

		QString storedRefresh, storedAccount;
		if (decodeStoredToken(SecretVault::read(vaultKey()), &storedRefresh, &storedAccount))
		{
			Log::log() << "BrowserTokenProvider: renewing from the stored refresh token" << std::endl;
			m_account    = storedAccount;
			m_refreshing = true;
			m_flow->setRefreshToken(storedRefresh);
			m_flow->refreshTokens();   // tokenChanged() on success, refreshFailed() on rejection
			return;
		}
	}

	beginSignIn();
}

void BrowserTokenProvider::beginSignIn()
{
	// Fresh attempt: forget the previous failure so its message cannot suppress
	// (or leak into) this one.
	m_serverError.clear();

	// Bind loopback explicitly. Qt's loopback fallback only triggers for a *null*
	// address, but listen()'s default is a non-null one — so a no-argument call
	// binds 0.0.0.0, which exposes this redirect endpoint to the local network and
	// triggers a Windows Firewall prompt for a listener that only ever needs the
	// browser on this machine. The port is ephemeral unless the configuration
	// pins one (authRedirectPort): Entra ignores it for loopback redirects, but
	// Okta matches it exactly — which is why the pin exists.
	m_replyHandler->close();

	const quint16 port = m_redirectPort > 0 ? quint16(m_redirectPort) : quint16(0);
	if (!m_replyHandler->listen(QHostAddress::LocalHost, port))
	{
		fail(m_redirectPort > 0
				? QStringLiteral("Could not open port %1 to receive the sign-in redirect — it may "
								  "already be in use. Close whatever holds it, or clear the redirect "
								  "port setting to pick a free port automatically.").arg(m_redirectPort)
				: QStringLiteral("Could not open a local port to receive the sign-in redirect. "
								  "JASP needs to listen on 127.0.0.1 to complete sign-in."));
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
			   << " (" << describeToken(accessToken).toStdString() << ")" << std::endl;

	// Persist the refresh token so the next JASP run renews silently instead of
	// opening the browser. Written on every acquisition because Entra rotates
	// refresh tokens on use — an old one stops working.
	//
	// Degrade::Never: a refresh token must never land in obfuscated storage, so
	// on a machine with no OS vault the write fails and we say so below. The
	// session still works — it just won't survive a restart.
	const QString refresh = m_flow->refreshToken();
	if (!refresh.isEmpty()
	 && !SecretVault::write(vaultKey(), encodeStoredToken(refresh, m_account), SecretVault::Degrade::Never))
		Log::log() << "BrowserTokenProvider: no secure credential store is available — "
				  "sign-in works but will not persist across JASP restarts" << std::endl;

	emit tokenReady(accessToken);
}

void BrowserTokenProvider::onServerError(const QString &error, const QString &description)
{
	// Entra returns these percent-encoded; decode before showing them to anyone.
	const QString message = decodeUrlComponent(
		description.isEmpty() ? error : error + QStringLiteral(": ") + description).trimmed();

	// Qt emits this *before* the generic requestFailed() for the same failure, and
	// this is the one carrying the provider's own explanation (for Entra, the
	// AADSTS code and its reason). Remember it so the generic signal cannot
	// overwrite it in the UI — see onRequestFailed().
	m_serverError = message;
	fail(message);
}

void BrowserTokenProvider::onRequestFailed(const QString &reason)
{
	if (!m_serverError.isEmpty())
	{
		// The provider already explained what went wrong; the generic follow-up
		// adds nothing and would only hide the detail.
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

	// Memory only. The vault entry for this configuration deliberately survives
	// a provider switch — switching back should not require signing in again —
	// and is removed only when the stored token is actually rejected
	// (refreshFailed()).
}

QString BrowserTokenProvider::vaultKey() const
{
	return SecretVault::key({ QStringLiteral("AI"), QStringLiteral("oidc") }, m_configSignature);
}

void BrowserTokenProvider::refreshFailed(const QString &reason, bool networkError)
{
	m_refreshing = false;
	cancelAttempt();

	// Whatever went wrong, this stored token is not coming back.
	SecretVault::remove(vaultKey());
	Log::log() << "BrowserTokenProvider: stored refresh token rejected — "
			   << reason.toStdString() << std::endl;

	if (networkError)
	{
		// Opening a browser against the same unreachable network helps no one.
		fail(reason);
		return;
	}

	// The token is dead but the user is still there: go interactive rather than
	// surfacing an error they cannot act on.
	beginSignIn();
}
