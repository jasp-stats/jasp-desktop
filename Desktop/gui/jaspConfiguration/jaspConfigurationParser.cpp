#include "jaspConfigurationParser.h"
#include "jaspConfFormatParser.h"
#include "log.h"


JASPConfigurationParser *JASPConfigurationParser::getParser(const Format format)
{
	//add cases here as more formats/parsers are added
	return new JASPConfFormatParser();

}

JASPConfigurationParser::JASPConfigurationParser()
{

}
