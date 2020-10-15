//
// Copyright (C) 2018 University of Amsterdam
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

#ifndef _CODEPAGE_CONVERT_H_
#define _CODEPAGE_CONVERT_H_

#ifdef _WIN32 //TODO: what is the define for VS compiler
#define strcasecmp(a,b) stricmp(a,b)
#endif


#include <QTextCodec>
#include <QSet>

/**
 * Template class for converting
 * between charactopr sets.
 *
 * The ianaCSNameDest value (see Ctor()) MUST be compatible with the type defined.
 *
 */
class CodePageConvert
{
public:

	/**
	 * @brief _CodePageConvert CTor
	 * @param ianaCSNameSrc The IANA name of the source we need to read from.
	 */
	CodePageConvert(const char *ianaCSNameSrc);

	virtual ~CodePageConvert();

	/**
	 * @brief convertCodePage Converts a string from one codepage (ctor source) to another (ctor destination).
	 * @param instring The string to convert.
	 * @param strLen The length of the passed string.
	 * @return The converted string.
	 */
	std::string convertCodePage(const std::string &instring) const;
	std::string convertCodePage(const char *instring, size_t strLen) const;

	/**
	 * @brief findIanaName Finds an IANA name for the character code field in SPSS files.
	 * @param character_code The SPSS file value
	 * @return A IANA name (see @link http://www.iana.org/assignments/character-sets/character-sets.xhtml @endlink)
	 */
	static QByteArray findIANANameFromSPSS(int32_t character_code);


private:

	static QSet<QByteArray> _knownCPs;

	QTextDecoder	*_source;
};



#endif // SPSSCPCONVERT_H
