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

#include "codepageconvert.h"
#include <stdexcept>

using namespace std;

QSet<QByteArray> CodePageConvert::_knownCPs;

/**
 * @brief _CodePageConvert CTor
 * @param ianaCSNameSrc The IANA name of the source we need to read from.
 */
CodePageConvert::CodePageConvert(const char *ianaCSNameSrc)
 : _source(0)
{
	static const string msg("Cannot find charactor set ");

	// build the list of known names.
	if (_knownCPs.size() == 0)
	{
		QList<QByteArray> cps = QTextCodec::availableCodecs();
		for (QList<QByteArray>::const_iterator i = cps.begin(); i != cps.end(); i++)
			_knownCPs.insert(*i);
	}

	// Just copy if no conversion required.
	if (strcasecmp("utf-8", ianaCSNameSrc) != 0 && strcasecmp("ascii", ianaCSNameSrc) != 0)
	{
		// do we know of this codec?
		if (_knownCPs.find(ianaCSNameSrc) != _knownCPs.end())
			_source = QTextCodec::codecForName(ianaCSNameSrc)->makeDecoder();
		else
			throw runtime_error(msg + ianaCSNameSrc);
	}
}

CodePageConvert::~CodePageConvert()
{
	if (_source != 0)
		delete _source;
}

/**
 * @brief convertCodePage Converts a string from one codepage (ctor souce) to another (ctor destination).
 * @param instring The string to convert.
 * @return The converted string.
 */
string CodePageConvert::convertCodePage(const string &instring) const
{

	if (_source != 0)
	{

		QString temp1 =  _source->toUnicode(instring.c_str(), instring.size());
		QByteArray temp2 = temp1.toUtf8();
		return string(temp2.data(), temp2.size());
	}
	else
		return instring;
}

string CodePageConvert::convertCodePage(const char *instring, size_t strLen) const
{
	return convertCodePage( string(instring, strLen) );
}


/**
 * @brief findIanaName Finds an IANA name for the character code field in SPSS files.
 * @param character_code The SPSS file value
 * @return A IANA name (see @link http://www.iana.org/assignments/character-sets/character-sets.xhtml @endlink)
 */
QByteArray CodePageConvert::findIANANameFromSPSS(int32_t character_code)
{
	QByteArray result;
	switch(character_code)
	{
	case 1:
		result += "ebcdic-us";	// IBM US/Canada/Nederland.
		break;

	case 2:
	case 20127: // Windows code page for 7 bit ASCII.
		result += "utf-8";	// 7 Bit ASCII maps directly to UTF-8, and requires no conversion.
		break;

	case 3:	// 8 bit ascii - probably CP 437.
		return findIANANameFromSPSS(437);
		break;

	case 437: // IBM code pages.
	case 500:
	case 851:
	case 852:
	case 855:
	case 857:
	case 860:
	case 861:
	case 863:
	case 864:
	case 865:
	case 868:
	case 869:
	case 870:
	case 871:
	case 880:
	case 891:
	case 903:
	case 904:
	case 918:

		result += "IBM";
		result += QByteArray::number(character_code);
		break;

	case 1250:
	case 1251:
	case 1252:
	case 1253:
	case 1254:
	case 1255:
	case 1256:
	case 1257:
	case 1258:
		result += "windows-";
		result += QByteArray::number(character_code);
		break;

	case 28591:	// IS0 8859...
	case 28592:
	case 28593:
	case 28594:
	case 28595:
	case 28596:
	case 28597:
	case 28598:
	case 28599:
	case 28603:
	case 28605:
		result += "ISO-8859-";
		result += QByteArray::number(character_code - 28590);
		break;

	case 65000: // UTF-7
	case 65001: // UTF-8
		result += "UTF-";
		result += QByteArray::number(character_code - 64993);
		break;

	default:
		result = "UTF-8";
	}

	return result;
}

