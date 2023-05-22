#include "jaspconfigurationparser.h"
#include "jaspconfigurationtomlparser.h"
#include "log.h"


JASPConfigurationParser* JASPConfigurationParser::getParser(const Format format)
{
	//add cases here as more formats/parsers are added
	return new JASPConfigurationTOMLParser();

}

JASPConfigurationParser::JASPConfigurationParser()
{

}
