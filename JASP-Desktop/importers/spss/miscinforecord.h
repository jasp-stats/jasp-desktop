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

#ifndef MISCINFORECORD_H
#define MISCINFORECORD_H

#include "datainforecord.h"

namespace spss
{

/**
 * @brief The DataInfoRecordVarSub class
 *
 * Handles info record of sub type "other".
 */
class MiscInfoRecord : public ReadableRecord<rectype_meta_data>
{
public:

	/**
	 * @brief MiscInfoRecord::MiscInfoRecord
	 * @param const Converters &fixer	Endain Fixer.
	 * @param fileSubType the record sub type from file.
	 * @param fileType The record type from file
	 * @param fromStream The file to read.
	 */
	MiscInfoRecord(const NumericConverter &fixer, int32_t fileSubType, RecordTypes fileType, SPSSStream &fromStream);

	SPSSIMPORTER_READ_ATTRIB(int32_t, size)
	SPSSIMPORTER_READ_ATTRIB(int32_t, count)
	SPSSIMPORTER_READ_ATTRIB(std::string, data)


	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);
};

}

#endif // MISCINFORECORD_H
