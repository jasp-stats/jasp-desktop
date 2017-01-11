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

#ifndef LONGVARNAMESRECORD_H
#define LONGVARNAMESRECORD_H

#include "datainforecord.h"

namespace spss
{

/**
 * @brief The LongVarNamesRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_longvar = 13,
 */
class LongVarNamesRecord : public DataInfoRecord<recsubtype_longvar>
{
public:
	/**
	 * @brief LongVarNamesRecord Ctor
	 * @param const Converters &fixer - Endain fixer.
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 */
	LongVarNamesRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	// TODO: Break up the name / var pairs.
	// Not currently required for JASP.
	SPSSIMPORTER_READ_ATTRIB(std::string, var_name_pairs)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);

};

}

#endif // LONGVARNAMESRECORD_H
