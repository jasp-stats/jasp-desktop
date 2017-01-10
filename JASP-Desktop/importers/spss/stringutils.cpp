//
// Copyright (C) 2015-2017 University of Amsterdam
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

#include "stringutils.h"
#include <string.h>


using namespace std;

/*
 * String utility functions in for the ASV importer
 */

/**
 * @brief rTrimWS Strips whitespace from the right side of str.
 * @param str The string to strip.
 * @return A copy of the string stript.

 */

string StrUtils::rTrimWS(const string &str)
{
	string result = str;
	rTrimWSIP(result);
	return result;
}

/**
 * @brief rTrimWS Strips whitespace from the right side of str.
 * @param str The string to strip.
 * @return A copy of the string stript.

 */

void StrUtils::rTrimWSIP(string &str)
{
	if (str.length() == 0)
		return;
	else
		while (isspace(str[str.size() -1]))
			str = str.substr(0, str.length() - 1);
}

/**
 * @brief rTrimWS Strips whitespace inplace from the right side of str
 * @param str The buffer to strip.
 * @param end End position of input string.
 */
void StrUtils::rTrimWSIP(char *str, size_t end)
{
	char * pos = str + end;
	while ((pos != 0) && !(str > pos))
	{
		if ( isspace(*pos))
			*pos-- = '\0';
		else
			pos = 0;
	}
}

/**
* @brief lTrimWS Strips whitespace from the left side of str.
* @param str The string to strip.
* @return A copy of the string stript.
*/
string StrUtils::lTrimWS(const std::string &str)
{
	string result = str;
	lTrimWSIP(result);
	return result;
}

/**
* @brief lTrimWS Strips whitespace from the left side of str, in place.
* @param str The string to strip.
*/
void StrUtils::lTrimWSIP(std::string &str)
{
	if (str.length() == 0)
		return;
	else
		while (isspace(str[0]) )
			str = str.substr(1);
}

/**
* @brief lTrimWS Strips whitespace in place from the left side of str
* @param str The buffer to strip.
*/
void StrUtils::lTrimWSIP(char *str)
{
	if (str != 0)
	{
		if (isspace(*str))
		{
			char *pos = str;
			do
			{
				pos++;
				*(pos - 1) = *pos;
			} while (*pos != '\0');

		}
	}
}
