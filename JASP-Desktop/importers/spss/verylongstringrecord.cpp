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

#include "verylongstringrecord.h"
#include "spssimportdataset.h"

#include "../importerutils.h"

using namespace std;
using namespace spss;


/**
 * @brief VarDisplayParamRecord Ctor
 * @param fixer The endainness fixer.
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param fromStream The file to read from.
 */
VeryLongStringRecord::VeryLongStringRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	:DataInfoRecord(fixer, fileSubType, fileType, from)
{
	// Read string lengths.
	size_t len = size() * count();
	{
		char * buffer = new char[len + 1];
		from.read(buffer, len);
		buffer[len] = '\0';
		_string_lengths = string(buffer, len);
		delete [] buffer;
	}
}


/**
 * @brief process Manipulates columns by adding the contents of thie record.
 *
 * Implematations should examine columns to determine the record history.
 */
void VeryLongStringRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	SPSSImportDataSet::LongColsData strLengths;

	{
		Tuples strLens = breakNamePairs(_string_lengths);
		for (Tuples::const_iterator i = strLens.begin(); i != strLens.end(); i++)
			strLengths.insert(pair<string, size_t>(i->first, atol(i->second.c_str())));
	}
	dataset->veryLongColsDat(strLengths);

#ifndef QT_NO_DEBUG
//	DEBUG_COUT3("Found ", strLengths.size(), " tuples:");
//	for (map<string, size_t>::const_iterator i = strLengths.begin(); i != strLengths.end(); i++)
//	{
//		DEBUG_COUT5("... name = \"", i->first, "\" value = ", i->second, ".");
//	}
#endif

}

