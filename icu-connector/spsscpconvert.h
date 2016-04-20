#ifndef SPSSCPCONVERT_H
#define SPSSCPCONVERT_H

#include "icuconnector.h"

namespace spss
{

class SpssCPConvert
{
public:

public:
	/**
	 * @brief SpssCPConvert Ctor
	 * @param CodePage source - The source code page.
	 * @param CodePage destination - The destination code page.
	 *
	 */
	SpssCPConvert(ICUConnector::CodePage source, ICUConnector::CodePage destination = ICUConnector::utf_8);

	~SpssCPConvert();

	/**
	 * @brief convertCodePage Converts a string from one codepage (ctor source) to another (ctor destination).
	 * @param instring The string to convert.
	 * @param strLen The length of the passed string.
	 * @return The converted string.
	 */
	std::string fwdConvertCodePage(const std::string &instring) const;
	std::string fwdConvertCodePage(const char *instring, size_t strLen) const;

	/**
	 * @brief convertCodePage Converts a string from one codepage (ctor destination) to another (ctor source).
	 * @param instring The string to convert.
	 * @param strLen The length of the passed string.
	 * @return The converted string.
	 */
	std::string revConvertCodePage(const std::string &instring) const;
	std::string revConvertCodePage(const char *instring, size_t strLen) const;

	/**
	 * @brief findCodepage Finds a code page from the .SAV file value.
	 * @param character_code character_code from the Machine Integer Info Record.
	 * @return _unknown_codePage on fail, other value on success.
	 */
	static ICUConnector::CodePage findCodepage(int32_t character_code);

private:

	ICUConnector	*_source;
	ICUConnector	*_destination;
};

} // End namepsatce SPSS.

#endif // SPSSCPCONVERT_H
