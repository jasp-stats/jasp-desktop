/*
	Copyright (C) Copyright (C) 2013-2016 University of Amsterdam

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.


	File created by patrick, on 16-09-2016,
	for class ConvertedStringContainer

*/


#ifndef CONVERTEDSTRINGCONTAINER_H
#define CONVERTEDSTRINGCONTAINER_H

#include "codepageconvert.h"


#include <set>

class ConvertedStringContainer
{
public:

	/**
	  * @brief RecordRoot default Ctor
	  *
	  * When Constructing a FileHeaderRecord fileHEader is 0.
	  */
	ConvertedStringContainer();

	ConvertedStringContainer(const 	ConvertedStringContainer &that);

	~ConvertedStringContainer();


	/**
	 * @brief processAllStrings Calls processStrings(const SpssCPConvert) on all memeber of _stringholders.
	 * @param converter The convertor to pass on.
	 */
	static void processAllStrings(const CodePageConvert &converter);

protected:
	/**
	 * @brief processStrings Converts any strings in the data fields.
	 * @param dictData The
	 *
	 * Should be implemented in classes where holdStrings maybe or is true.
	 *
	 */
	virtual void processStrings(const CodePageConvert &converter) = 0;

private:
	static void _addOne(ConvertedStringContainer *newOne);

	static std::set<ConvertedStringContainer *> * _convertContainers; /** < Holds all instances where holdsStrings == true */
};


#endif // CONVERTEDSTRINGCONTAINER_H
