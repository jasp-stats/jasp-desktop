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

#ifndef VARDISPLAYPARAMRECORD_H
#define VARDISPLAYPARAMRECORD_H



#include "datainforecord.h"

namespace spss {

/**
 * @brief The VarDisplayParamRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_display = 11,
 */
class VarDisplayParamRecord : public DataInfoRecord<recsubtype_display>
{
public:

	class DisplayParams
	{
	public:

		/**
		 * @brief VarDisplayRecord Ctor
		 * @param const Converters &fixer - Endain fixer.
		 * @param useWidth True if the width vlaue is read.
		 * @param fromStream The file to read from.
		 */
		DisplayParams(const NumericConverter &fixer, bool readWidth, SPSSStream &from);

		SPSSIMPORTER_READ_ATTRIB(int32_t, measure)
		SPSSIMPORTER_READ_ATTRIB(int32_t, width)
		SPSSIMPORTER_READ_ATTRIB(int32_t, alignment)
	};

	/**
	 * @brief VarDisplayParamRecord Ctor
	 * @param const Converters &fixer - Endain fixer.
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param numColumns The number of columns discovered (to date)
	 * @param fromStream The file to read from.
	 */
	VarDisplayParamRecord(const NumericConverter &fixer, RecordSubTypes fileSubType,
						  RecordTypes fileType, int32_t numCoumns, SPSSStream &from);

	std::vector<DisplayParams>  _displayParams;
	const std::vector<DisplayParams>& displayParams()
	{
		return _displayParams;
	}

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);

};

}

#endif // VARDISPLAYPARAMRECORD_H
