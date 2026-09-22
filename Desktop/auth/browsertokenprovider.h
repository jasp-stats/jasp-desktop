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
// The refresh token is persisted through SecretVault (Credential Manager on
// Windows, Keychain on macOS, Secret Service on Linux where present), so a
// JASP restart renews silently instead of opening the browser again. Written
// with Degrade::Never — on a machine with no OS vault the write fails, is
// logged, and sign-in simply repeats each run. Within a run Qt renews ahead
// of expiry (autoRefresh + refreshLeadTime).
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

	/// Push the OIDC settings this provider should use. The feature that owns
	/// us calls this whenever its configuration changes; nothing here reads
	/// any feature's settings — auth/ must stay independent of its callers.
	/// An empty clientId means "the JASP default registration".
	void setOidcConfig(const OidcConfig &config);

	/// The JASP app registration, used when the provider config carries no
	/// authClientId of its own. A customer that registers its own app sets one.
	static const QString &defaultClientId();

private:
	/// Point the flow at the pushed configuration. Returns false and sets
	/// *error when there is not enough to sign in with.
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

	/// The vault key for the current configuration. Empty only before the first
	/// configure(), which no caller can reach.
	QString vaultKey() const;

	/// A silent renewal from a stored refresh token was rejected. Forget the
	/// stored token; then either fall back to interactive sign-in (the token was
	/// revoked — the browser will produce a fresh one) or surface the failure
	/// (the network is down, and a browser would not help either).
	void refreshFailed(const QString &reason, bool networkError);

	QOAuth2AuthorizationCodeFlow *m_flow         = nullptr;
	QOAuthHttpServerReplyHandler *m_replyHandler = nullptr;
	QTimer                       *m_timeout      = nullptr;

	QString m_account;
	QString m_authority;         // pushed via setOidcConfig
	QString m_scope;
	QString m_clientId;
	QString m_configSignature;   // authority|scope|clientId the cached token belongs to
	QString m_serverError;       // the provider's own explanation for the current attempt
	bool    m_running    = false;
	bool    m_vaultTried = false;  // the vault was consulted for this configuration
	bool    m_refreshing = false;  // a silent renewal from a stored token is in flight
};

#endif // BROWSERTOKENPROVIDER_H
