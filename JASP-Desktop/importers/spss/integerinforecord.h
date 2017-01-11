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

#ifndef INTEGERINFORECORD_H
#define INTEGERINFORECORD_H

#include "datainforecord.h"

namespace spss
{


/**
 * @brief The IntegerInfoRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_integer = 3,
 */
class IntegerInfoRecord : public DataInfoRecord<recsubtype_integer>
{
public:

	/**
	 * @brief IntegerInfoRecord default Ctor
	 */
	IntegerInfoRecord();

	/**
	 * @brief IntegerInfoRecord Ctor
	 * @param Converters fixer Fixes endainness.
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 *
	 * NB This constructor will modify the contents of fixer!
	 */
	IntegerInfoRecord(NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &fromStream);

	virtual ~IntegerInfoRecord();

	/*
	 * Data!
	 */
	SPSSIMPORTER_READ_ATTRIB(int32_t, version_major)
	SPSSIMPORTER_READ_ATTRIB(int32_t, version_minor)
	SPSSIMPORTER_READ_ATTRIB(int32_t, version_revision)
	SPSSIMPORTER_READ_ATTRIB(int32_t, machine_code)
	SPSSIMPORTER_READ_ATTRIB(int32_t, floating_point_rep)
	SPSSIMPORTER_READ_ATTRIB(int32_t, compression_code)
	SPSSIMPORTER_READ_ATTRIB(int32_t, endianness)
	SPSSIMPORTER_READ_ATTRIB(int32_t, character_code)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);
};

}

#endif // INTEGERINFORECORD_H
