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
	while ( isspace(result[result.size() -1]) )
		result = result.substr(0, result.length() - 1);
	return result;
}

/**
 * @brief rTrimWS Strips whitespace from the right side of str.
 * @param str The string to strip.
 * @return A copy of the string stript.

 */

void StrUtils::rTrimWSIP(string &str)
{
	while ( isspace(str[str.size() -1]) )
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

