//
// ApiKeyTokenProvider — TokenProvider for static API keys.
//
// The key is pushed in by the feature that owns the provider; there is no
// cache, so pushing a new key takes effect on the next request. No
// interactive step is ever required.
//

#ifndef APITOKENPROVIDER_H
#define APITOKENPROVIDER_H

#include "tokenprovider.h"

class ApiKeyTokenProvider : public TokenProvider
{
	Q_OBJECT

public:
	explicit ApiKeyTokenProvider(QObject *parent = nullptr);

	QString   authMode() const override;
	void      ensureToken() override;
	QString   token() const override;
	bool      isValid() const override;
	QDateTime expiresAt() const override;
	QString   accountName() const override;
	void      signOut() override;

	/// The key to use as the token. May be empty, in which case isValid()
	/// is false and requests go out unauthenticated.
	void setApiKey(const QString &key) { m_apiKey = key; }

private:
	QString m_apiKey;
};

#endif // APITOKENPROVIDER_H
