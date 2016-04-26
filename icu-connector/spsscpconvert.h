//
// Copyright (C) 2016 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

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
