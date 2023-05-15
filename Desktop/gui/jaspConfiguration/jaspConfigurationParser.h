#ifndef JASPCONFIGURATION_PARSER_H
#define JASPCONFIGURATION_PARSER_H

#include "jaspconfiguration.h"

class JASPConfigurationParser
{

public:
	enum class Format {
		JASP
	};

	//factory function
	static JASPConfigurationParser* getParser(const Format format);

	virtual bool parse(JASPConfiguration* target, const QString& input) = 0;

protected:
	JASPConfigurationParser();
};

#endif // JASPCONFIGURATION_PARSER_H
