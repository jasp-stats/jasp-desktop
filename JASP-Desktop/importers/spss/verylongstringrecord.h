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

#ifndef VERYLONGSTRINGRECORD_H
#define VERYLONGSTRINGRECORD_H




#include "datainforecord.h"

namespace spss {

/**
 * @brief The VarDisplayParamRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type verylongstr = 14.
 */
class VeryLongStringRecord : public DataInfoRecord<recsubtype_verylongstr>
{
public:

	/**
	 * @brief VarDisplayParamRecord Ctor
	 * @param const Converters &fixer	Endain Fixer.
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 */
	VeryLongStringRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	// The name value pairs we got.
	SPSSIMPORTER_READ_ATTRIB(std::string, string_lengths);


	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);
};

} // End namespace spss

#endif // VERYLONGSTRINGRECORD_H
