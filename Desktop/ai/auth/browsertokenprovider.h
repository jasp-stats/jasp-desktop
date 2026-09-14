//
// BrowserTokenProvider — OIDC sign-in through the user's own system browser.
//
// Authorization code grant with PKCE (S256) over a loopback redirect, using
// QtNetworkAuth. This is the backend JASP ships.
//
// Nothing here is Entra-specific: authority, scope and client id all come from
// AIConfigModel, so pointing JASP at another OIDC provider is configuration
// rather than code (see the plan §4). That is also why no third-party binary is
// involved — see tokenprovider.h for why the Windows broker is not an option.
//
// Tokens live in memory only for now. The refresh token belongs in the OS vault
// (DPAPI / Keychain / libsecret) and that is a separate step, so today the user
// signs in once per JASP run. Within a run Qt renews silently ahead of expiry
// (autoRefresh + refreshLeadTime), and on a new run the browser session usually
// means no prompt.
//

#ifndef BROWSERTOKENPROVIDER_H
#define BROWSERTOKENPROVIDER_H

#include "tokenprovider.h"

#include <QDateTime>
#include <QString>

class QOAuth2AuthorizationCodeFlow;
class QOAuthHttpServerReplyHandler;
class QTimer;

class BrowserTokenProvider : public TokenProvider
{
	Q_OBJECT

public:
	explicit BrowserTokenProvider(QObject *parent = nullptr);
	~BrowserTokenProvider() override;

	QString   authMode() const override;
	void      ensureToken() override;
	QString   token() const override;
	bool      isValid() const override;
	QDateTime expiresAt() const override;
	QString   accountName() const override;
	void      signOut() override;

	/// The JASP app registration, used when the provider config carries no
	/// authClientId of its own. A customer that registers its own app sets one.
	static const QString &defaultClientId();

private:
	/// Re-read authority/scope/client id and (re)point the flow. Returns false
	/// and sets *error when the provider is not configured enough to sign in.
	bool configure(QString *error);

	/// Open the browser and wait for the loopback redirect.
	void beginSignIn();

	/// Stop the listener and drop any in-flight attempt.
	void cancelAttempt();

	void onGranted();
	void onTokenChanged(const QString &accessToken);
	void onServerError(const QString &error, const QString &description);
	void onRequestFailed(const QString &reason);
	void fail(const QString &message);

	QOAuth2AuthorizationCodeFlow *m_flow         = nullptr;
	QOAuthHttpServerReplyHandler *m_replyHandler = nullptr;
	QTimer                       *m_timeout      = nullptr;

	QString m_account;
	QString m_configSignature;   // authority|scope|clientId the cached token belongs to
	bool    m_running    = false;
	bool    m_refreshing = false;
};

#endif // BROWSERTOKENPROVIDER_H
