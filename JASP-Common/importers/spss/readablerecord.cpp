//
// Copyright (C) 2015-2016 University of Amsterdam
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
#include "readablerecord.h"

using namespace std;
using namespace spss;

set<RecordRoot *> *RecordRoot::_pRecords = 0;

RecordRoot::RecordRoot()
{

	if (_pRecords == 0)
		_pRecords = new set<RecordRoot *>();

	// Keep track of instances.
	_pRecords->insert(this);
}


RecordRoot::~RecordRoot()
{
	set<RecordRoot *>::iterator iter = _pRecords->find(this);
	if ( iter != _pRecords->end())
		_pRecords->erase(iter);

	if (_pRecords->size() == 0)
	{
		delete _pRecords;
		_pRecords = 0;
	}
}

/**
 * @brief processAllStrings Calls processStrings(const SpssCPConvert) on all memeber of _records.
 * @param converter The convertor to pass on.
 */
void RecordRoot::processAllStrings(const SpssCPConvert &converter)
{
	for(set<RecordRoot *>::iterator iter = _pRecords->begin();
		iter != _pRecords->end();
		++iter)
	{
		(*iter)->processStrings(converter);
	}

}
