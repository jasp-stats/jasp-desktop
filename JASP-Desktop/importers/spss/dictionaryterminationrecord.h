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

#ifndef DICTIONARYTERMINATIONRECORD_H
#define DICTIONARYTERMINATIONRECORD_H



#include "readablerecord.h"

namespace spss
{

/**
 * @brief The DictionaryTermination class Deacode diationery termination (place holder)
 *
 * Assocateed with record type  rectype_dict_term = 999
 *
 */
class DictionaryTermination : public ReadableRecord<rectype_dict_term>
{
public:

	/**
	 * @brief DictionaryTermination Ctor
	 * @brief Converters &fixer _ Converts endiness (if required)
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream File to read from
	 */
	DictionaryTermination(const NumericConverter &fixer, RecordTypes fileType, SPSSStream &fromStream);

    virtual ~DictionaryTermination();

	/*
	 * Dummy data.
	 */
    SPSSIMPORTER_READ_ATTRIB(int32_t, filler);

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);
};

}
#endif // DICTIONARYTERMINATIONRECORD_H
