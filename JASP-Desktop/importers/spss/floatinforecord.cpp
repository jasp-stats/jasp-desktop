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

#include "float.h"
#include "floatinforecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief FloatInfoRecord Ctor
 */
FloatInfoRecord::FloatInfoRecord()
	: DataInfoRecord(FloatInfoRecord::SUB_RECORD_TYPE, FloatInfoRecord::RECORD_TYPE)
	, _sysmis( NAN )
	, _highest( DBL_MIN )
	, _lowest( DBL_MAX )
{
}

/**
 * @brief FloatInfoRecord Ctor
 * @param const Converters &fixer Fixes endainness.
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 */
FloatInfoRecord::FloatInfoRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fixer, fileSubType, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(sysmis, from, fixer);
	SPSSIMPORTER_READ_MEMBER(highest, from, fixer);
	SPSSIMPORTER_READ_MEMBER(lowest, from, fixer);
}

FloatInfoRecord::~FloatInfoRecord()
{

}

void FloatInfoRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	// Do Nohting.
}
