#include "apitokenprovider.h"

ApiKeyTokenProvider::ApiKeyTokenProvider(QObject *parent)
	: TokenProvider(parent)
{}

QString ApiKeyTokenProvider::authMode() const
{
	return QStringLiteral("apiKey");
}

void ApiKeyTokenProvider::ensureToken()
{
	// A static key needs no asynchronous acquisition; report it immediately.
	// Later interactive backends will emit this after sign-in instead.
	emit tokenReady(token());
}

QString ApiKeyTokenProvider::token() const
{
	return m_apiKey;
}

bool ApiKeyTokenProvider::isValid() const
{
	return !token().isEmpty();
}

QDateTime ApiKeyTokenProvider::expiresAt() const
{
	// A static key has no expiry we can see, and none we could renew.
	return {};
}

QString ApiKeyTokenProvider::accountName() const
{
	// An API key identifies the key's owner, not a signed-in user.
	return {};
}

void ApiKeyTokenProvider::signOut()
{
	// Nothing to revoke for a static key. Clearing it is the user's action via
	// the API-key field, not a sign-out.
}
