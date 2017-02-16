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


#include "vardisplayparamrecord.h"
#include "../importerutils.h"
#include "spssimportdataset.h"

using namespace std;
using namespace boost;
using namespace spss;


/**
 * @brief VarDisplayParamRecord Ctor
 * @param fixer - The Endainness fixer
 * @param useWidth True if we use the width.
 * @param fromStream The file to read from.
 */
VarDisplayParamRecord::DisplayParams::DisplayParams(const NumericConverter &fixer, bool usedWidth, SPSSStream &from)
 : _width(0)
{
	SPSSIMPORTER_READ_MEMBER(measure, from, fixer);
	if (usedWidth)
	{
		SPSSIMPORTER_READ_MEMBER(width, from,fixer);
	}
	SPSSIMPORTER_READ_MEMBER(alignment, from,fixer);
};



/**
 * @brief VarDisplayRecord Ctor
 * @param fixer - The Endainness fixer
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param numCoumns The number of columns discovered (to date)
 * @param fromStream The file to read from.
 */
VarDisplayParamRecord::VarDisplayParamRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, int32_t numColumns, SPSSStream &from)
	: DataInfoRecord(fixer, fileSubType, fileType, from)
{
	// do we read the width value?
	bool useWidth = count() > (2 * numColumns);

	// Fetch all the records.
	for (int i = 0; i < numColumns; i++)
	{
		DisplayParams dp(fixer, useWidth, from);
		_displayParams.push_back( dp );
	}
}


/**
 * @brief process Manipulates columns by adding the contents of thie record.
 *
 * Implematations should examine columns to determine the record history.
 */
void VarDisplayParamRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	// String continuation columns do not have a display params entry.
	size_t i = 0;
	for (ImportColumns::iterator iter = dataset->begin();
		 (iter != dataset->end()) && (i < _displayParams.size());
		 ++iter, ++i)
	{
		SPSSImportColumn *col = dynamic_cast<SPSSImportColumn*>(*iter);
		// place measure in this col.
		col->spssMeasure( static_cast<Measure>( _displayParams[i].measure()) );
	}
}
