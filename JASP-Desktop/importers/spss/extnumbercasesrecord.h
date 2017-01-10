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

#ifndef EXTNUMBERCASESRECORD_H
#define EXTNUMBERCASESRECORD_H

#include "datainforecord.h"
#include "numericconverter.h"

namespace spss {

/**
 * @brief The ExtNumberCasesRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_extnumcases = 16,
 */
class ExtNumberCasesRecord : public DataInfoRecord<recsubtype_extnumcases>
{
public:

	/**
	 * @brief ExtNumberCasesRecord Ctor
	 * @param const Converters &fixer Fixer for endianness.
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param from The file to read from.
	 */
	ExtNumberCasesRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	SPSSIMPORTER_READ_ATTRIB(int64_t, unknown);
	SPSSIMPORTER_READ_ATTRIB(int64_t, ncases64);

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implemetations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);
};


}

#endif // EXTNUMBERCASESRECORD_H
