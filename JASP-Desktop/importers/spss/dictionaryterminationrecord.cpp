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

#include "dictionaryterminationrecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief DictionaryTermination Ctor
 * @param fileType The record type value, as found in the file.
 * @param fromStream File to read from
 */
DictionaryTermination::DictionaryTermination(const NumericConverter &fixer, RecordTypes fileType, SPSSStream &from)
 : ReadableRecord(fixer, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(filler, from, fixer);
}

DictionaryTermination::~DictionaryTermination()
{

}

/**
 * @brief Does nothing
 *
 */
void DictionaryTermination::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	// inin the columns iterator.
	importer->resetNextCol(dataset);
}
