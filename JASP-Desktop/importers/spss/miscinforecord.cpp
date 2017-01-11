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


#include "miscinforecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief MiscInfoRecord::MiscInfoRecord
 * @param const Converters &fixer - Endain fixer.
 * @param fileSubType the record sub type from file.
 * @param fileType The record type from file
 * @param fromStream The file to read.
 */
MiscInfoRecord::MiscInfoRecord(const NumericConverter &fixer, int32_t fileSubType, RecordTypes fileType, SPSSStream &from)
	: ReadableRecord(fixer, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(size, from, fixer);
	SPSSIMPORTER_READ_MEMBER(count, from, fixer);
	{
		size_t sizeData = _size * _count;
		char * buffer = new char[sizeData + 2];
		from.read(buffer, sizeData);
		fixer.fixup(buffer, sizeData);
		_data.append(buffer, sizeData);
		delete[] buffer;
	}
}


/**
 * @brief process Manipulates columns by adding the contents of thie record.
 *
 * Implematations should examine columns to determine the record history.
 */
void MiscInfoRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{

}
