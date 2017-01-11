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



#include "extnumbercasesrecord.h"
#include "spssimportdataset.h"


using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief ExtNumberCasesRecord Ctor
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 */
ExtNumberCasesRecord::ExtNumberCasesRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fixer, fileSubType, fileType, from)
{
	// Go through the fields, just fetching as we go..
	SPSSIMPORTER_READ_MEMBER(unknown, from, fixer);
	SPSSIMPORTER_READ_MEMBER(ncases64, from, fixer);
};


void ExtNumberCasesRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{

	// Extract the number of cases.
	if (ncases64() != -1L)
		dataset->numCases(ncases64());
}
