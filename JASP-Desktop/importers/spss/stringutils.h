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


#ifndef __SPSS_STRING_UTILS_H_
#define __SPSS_STRING_UTILS_H_


#include <string>

/*
 * String utility functions in for the ASV importer
 */
class StrUtils
{
public:
	/**
	* @brief rTrimWS Strips whitespace from the right side of str.
	* @param str The string to strip.
	* @return A copy of the string stript.
	*/
	static std::string rTrimWS(const std::string &str);

	/**
	* @brief rTrimWS Strips whitespace from the right side of str, in place.
	* @param str The string to strip.
	*/
	static void rTrimWSIP(std::string &str);

	/**
	* @brief rTrimWS Strips whitespace in place from the right side of str
	* @param str The buffer to strip.
	* @param end End position of string.
	*/
	static void rTrimWSIP(char *str, size_t end);

	/**
	* @brief lTrimWS Strips whitespace from the left side of str.
	* @param str The string to strip.
	* @return A copy of the string stript.
	*/
	static std::string lTrimWS(const std::string &str);

	/**
	* @brief lTrimWS Strips whitespace from the left side of str, in place.
	* @param str The string to strip.
	*/
	static void lTrimWSIP(std::string &str);

	/**
	* @brief lTrimWS Strips whitespace in place from the left side of str
	* @param str The buffer to strip.
	*/
	static void lTrimWSIP(char *str);

};


#endif // Sentinal
