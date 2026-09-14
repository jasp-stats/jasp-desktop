//
// ApiKeyTokenProvider — TokenProvider for static API keys.
//
// This preserves the pre-existing behavior exactly: the token is the provider's
// configured API key, read live from AIConfigModel at request time (no cache),
// so edits take effect immediately. No interactive step is ever required.
//

#ifndef APITOKENPROVIDER_H
#define APITOKENPROVIDER_H

#include "tokenprovider.h"

class ApiKeyTokenProvider : public TokenProvider
{
	Q_OBJECT

public:
	explicit ApiKeyTokenProvider(QObject *parent = nullptr);

	QString authMode() const override;
	void    ensureToken() override;
	QString token() const override;
	bool    isValid() const override;
	void    signOut() override;
};

#endif // APITOKENPROVIDER_H
