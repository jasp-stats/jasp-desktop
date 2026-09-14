//
// TokenProvider — abstract source of the HTTP auth token used by AiBridge.
//
// AiBridge must never know *how* a token is obtained. Each backend owns
// acquisition, caching, refresh and sign-out, and reports the result through
// the signals below.
//
// Backends (see Docs/development/aiBridge/08_entra_auth_plan.md §3):
//   ApiKeyTokenProvider   static API key — today's behavior (Phase 1)
//   WamTokenProvider      Windows Web Account Manager (Phase 2)
//   BrowserTokenProvider  loopback PKCE via QtNetworkAuth (Phase 4)
//

#ifndef TOKENPROVIDER_H
#define TOKENPROVIDER_H

#include <QObject>
#include <QString>

class TokenProvider : public QObject
{
	Q_OBJECT

public:
	explicit TokenProvider(QObject *parent = nullptr) : QObject(parent) {}
	~TokenProvider() override = default;

	/// The auth scheme this provider implements: "apiKey", "oidc" or "none".
	/// Named by protocol rather than by vendor, so a new identity provider is a
	/// new backend rather than a new scheme.
	virtual QString authMode() const = 0;

	/// Begin (or refresh) token acquisition. Must not block the caller; report
	/// the outcome with tokenReady(), interactionRequired() or authFailed().
	virtual void ensureToken() = 0;

	/// Last known token; may be empty. Must never trigger acquisition.
	virtual QString token() const = 0;

	/// True while token() holds a usable value.
	virtual bool isValid() const = 0;

	/// Discard any cached credentials/tokens. Safe to call when signed out.
	virtual void signOut() = 0;

signals:
	void tokenReady(const QString &token);
	void interactionRequired(const QString &reason);
	void authFailed(const QString &error);
};

#endif // TOKENPROVIDER_H
