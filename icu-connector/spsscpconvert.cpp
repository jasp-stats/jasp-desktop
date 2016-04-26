#include "spsscpconvert.h"
#include "icuconnector.h"
#include <stdexcept>

using namespace std;
using namespace spss;

/**
 * @brief ICUConnector Ctor
 * @param CodePage source - The source code page.
 *
 * Proctected for Singleton behaviour.
 */
SpssCPConvert::SpssCPConvert(ICUConnector::CodePage source, ICUConnector::CodePage destination)
 : _source(ICUConnector::get(source))
 , _destination(ICUConnector::get(destination))
{

}

SpssCPConvert::~SpssCPConvert()
{

}

/**
 * @brief convertCodePage Converts a string from one codepage (ctor souce) to another (ctor destination).
 * @param instring The string to convert.
 * @return The converted string.
 */
string SpssCPConvert::fwdConvertCodePage(const string &instring) const
{
	// If source and destination are 100% compatible, just copy.
	if ((_destination->codepage() == _source->codepage())
        || ((_source->codepage()  == ICUConnector::utf_8)
            &&(_source->codepage()  == ICUConnector::us_ascii)) )
			return instring;
	else
		return _destination->convert(_source->convert(instring));
}

string SpssCPConvert::fwdConvertCodePage(const char *instring, size_t strLen) const
{
	return fwdConvertCodePage( string(instring, strLen) );
}


/**
 * @brief convertCodePage Converts a string from one codepage (ctor destination) to another (ctor source).
 * @param instring The string to convert.
 * @param strLen The length of the passed string.
 * @return The converted string.
 */
string SpssCPConvert::revConvertCodePage(const std::string &instring) const
{
	// If source and destination are 100% compatible, just copy.
    if ((_source->codepage() == _destination->codepage())
        || ((_destination->codepage()  == ICUConnector::utf_8)
            &&(_destination->codepage()  == ICUConnector::us_ascii)) )
			return instring;
	else
		return _source->convert(_destination->convert(instring));

}

string SpssCPConvert::revConvertCodePage(const char *instring, size_t strLen) const
{
	return revConvertCodePage( string(instring, strLen) );
}


/**
 * @brief findCodepage Finds a code page from the .SAV file value.
 * @param character_code character_code from the Machine Integer Info Record.
 * @return _unknown_codePage on fail, other value on success.
 */
ICUConnector::CodePage SpssCPConvert::findCodepage(int32_t character_code)
{
	if (character_code == 1) // "EBCDIC"
		throw runtime_error("Cannot convert from EBCDIC alphabet.");	// We are not going there!
	else if (character_code == 2) // 7 bit ASCII (aka US-ASCII)
		return ICUConnector::us_ascii;
	else if (character_code == 3) // "8-bit ASCII" (probably DOS437)
		return ICUConnector::dos437;
	else if (character_code == 4) // DEC Kanji;
		throw runtime_error("Cannot convert from DEC-Kanji (asian) alphabet.");	// We are not going there!
	else if (character_code == 65001)	// UTF-8
		return ICUConnector::utf_8;
	else if (character_code == 65000)	// UTF-7
		return ICUConnector::utf_7;
	// Windows (native) code pages...
	else if ((character_code >= ICUConnector::win1250) && (character_code <= ICUConnector::win1258))
		return (ICUConnector::CodePage) (character_code);
	// ISO 8599-1 varients.
	else if ((character_code >= ICUConnector::IS08859_1) && (character_code < ICUConnector::ISO8859_15))
		return (ICUConnector::CodePage) (character_code);
	// macintosh / with Code page.
	else if ((character_code > ICUConnector::macintosh) && (character_code <= ICUConnector::mac_hebrew))
		return (ICUConnector::CodePage) (character_code);
	else
		return ICUConnector::_unknown_codePage;
}
