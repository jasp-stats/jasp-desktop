/*
	Copyright (C) Copyright (C) 2013-2018 University of Amsterdam

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


	File created by patrick, on 23-09-2016
	Original file name was
*/

#ifndef CONVERTEDSTRINGCONTAINER_H
#define CONVERTEDSTRINGCONTAINER_H

#include "codepageconvert.h"

#include <set>

class ConvertedStringContainer
{
public:
	ConvertedStringContainer();
	ConvertedStringContainer(const ConvertedStringContainer &that);
	~ConvertedStringContainer();

	/**
	 * @brief processAllStrings Calls processStrings(const SpssCPConvert) on all memeber of _records.
	 * @param converter The convertor to pass on.
	 */
	static void processAllStrings(const CodePageConvert &converter);

	virtual void processStrings(const CodePageConvert &converter) = 0;

private:
	void _addOne(ConvertedStringContainer *newone);
	static std::set<ConvertedStringContainer *> *_convertContainers;
};

#endif // CONVERTEDSTRINGCONTAINER_H

