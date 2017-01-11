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

#ifndef FLOATINFORECORD_H
#define FLOATINFORECORD_H

#include "systemfileformat.h"
#include "datainforecord.h"

namespace spss {

/**
 * @brief The FloatInfoRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_float = 4,
 */
class FloatInfoRecord : public DataInfoRecord<recsubtype_float>
{
public:
	/**
	 * @brief FloatInfoRecord Ctor
	 */
	FloatInfoRecord();

	/**
	 * @brief FloatInfoRecord Ctor
	 * @param const Converters &fixer Fixes endainness.
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 */
	FloatInfoRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	virtual ~FloatInfoRecord();

	/*
	 * Data!
	 */
	SPSSIMPORTER_READ_ATTRIB(double, sysmis)
	SPSSIMPORTER_READ_ATTRIB(double, highest)
	SPSSIMPORTER_READ_ATTRIB(double, lowest)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);
};

}
#endif // FLOATINFORECORD_H
