/*
	Copyright (C) Copyright  (C) 2013-2018 University of Amsterdam

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


#include "convertedstringcontainer.h"

#include "importerutils.h"

using namespace std;

set<ConvertedStringContainer *> *ConvertedStringContainer::_convertContainers = 0;

ConvertedStringContainer::ConvertedStringContainer()
{
	_addOne(this);
}

ConvertedStringContainer::ConvertedStringContainer(const ConvertedStringContainer &that)
{
	_addOne(this);
}


ConvertedStringContainer::~ConvertedStringContainer()
{
	set<ConvertedStringContainer *>::iterator iter = _convertContainers->find(this);
	if ( iter != _convertContainers->end())
		_convertContainers->erase(iter);

	if (_convertContainers->size() == 0)
	{
		delete _convertContainers;
		_convertContainers = 0;
	}
}

/**
 * @brief processAllStrings Calls processStrings(const SpssCPConvert) on all memeber of _records.
 * @param converter The convertor to pass on.
 */
void ConvertedStringContainer::processAllStrings(const CodePageConvert &converter)
{
	for(set<ConvertedStringContainer *>::iterator iter = _convertContainers->begin();
		iter != _convertContainers->end();
		++iter)
	{
		(*iter)->processStrings(converter);
	}

}


void ConvertedStringContainer::_addOne(ConvertedStringContainer *newone)
{
	if (_convertContainers == 0)
		_convertContainers = new set<ConvertedStringContainer *>();

	// Keep track of instances.
	_convertContainers->insert(newone);
}
