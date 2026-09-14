//
// TokenProvider — abstract source of the HTTP auth token used by AiBridge.
//
// AiBridge must never know *how* a token is obtained. Each backend owns
// acquisition, caching, refresh and sign-out, and reports the result through
// the signals below.
//
// Backends (see Docs/development/aiBridge/08_entra_auth_plan.md §3):
//   ApiKeyTokenProvider   static API key — the default, unchanged behavior
//   BrowserTokenProvider  system browser + loopback PKCE via QtNetworkAuth
//
// The plan deliberately does not use the Windows broker (msalruntime): it is a
// closed binary whose redistribution terms are unclear, and JASP Desktop is
// AGPL3+. A future backend must likewise require no third-party binary, and must
// be addable here without AiBridge learning anything new.
//

#ifndef TOKENPROVIDER_H
#define TOKENPROVIDER_H

#include <QObject>
#include <QString>
#include <QDateTime>

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
	/// Safe to call when a valid token is already cached.
	virtual void ensureToken() = 0;

	/// Last known token; may be empty. Must never trigger acquisition.
	virtual QString token() const = 0;

	/// True while token() can still be used for a request. Implementations apply
	/// a safety margin, so a token about to expire counts as invalid and gets
	/// renewed rather than sent and rejected mid-request.
	virtual bool isValid() const = 0;

	/// When the cached token stops being usable. Invalid when unknown, or when
	/// nothing is cached. For UI and diagnostics.
	virtual QDateTime expiresAt() const = 0;

	/// The signed-in account, for display. Empty when there isn't one (an API
	/// key identifies the key's owner, not a user).
	virtual QString accountName() const = 0;

	/// Discard any cached credentials/tokens. Safe to call when signed out.
	/// Local only: this does not revoke anything at the identity provider.
	virtual void signOut() = 0;

signals:
	/// A usable token is available now.
	void tokenReady(const QString &token);

	/// The backend needs the user to do something before it can continue; the
	/// message is display-ready. Acquisition continues in the background, so a
	/// tokenReady() may still follow.
	void interactionRequired(const QString &message);

	/// Acquisition failed and will not complete without a new attempt. The
	/// string is display-ready.
	void authFailed(const QString &error);
};

#endif // TOKENPROVIDER_H
