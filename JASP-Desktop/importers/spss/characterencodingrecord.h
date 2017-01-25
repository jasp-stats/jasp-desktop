//
// Copyright (C) 2017 University of Amsterdam
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


#ifndef _CHARACTERENCODINGRECORD_H_
#define _CHARACTERENCODINGRECORD_H_

#include "datainforecord.h"

#include <map>

namespace spss {

/**
 * @brief The CharacterEncodingRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_charEncoding = 20,
 */
class CharacterEncodingRecord : public DataInfoRecord<recsubtype_charEncoding>
{
public:


	/**
	 * @brief CharacterEncodingRecord Ctor
	 * @param Converters fixer Fixes endainness.
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param from The file to read from.
	 *
	 * NB This constructor will modify the contents of fixer!
	 */
	CharacterEncodingRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	virtual ~CharacterEncodingRecord();

	/*
	 * Data!
	 */
	SPSSIMPORTER_READ_ATTRIB(int32_t, size)
	SPSSIMPORTER_READ_ATTRIB(int32_t, count)
	SPSSIMPORTER_READ_ATTRIB(std::string, encoding)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);

private:

	typedef std::map<std::string, std::string> _NameSubs;
	static _NameSubs _nameSubstitution; /**< character set name substitution. */

	/**
	 *  @brief _buildNs Builds the nameSubstitution map.
	 *  @return
	 *
	 */
	static _NameSubs _buildNs();

};

} // End name space.

#endif // CHARACTERENCODINGRECORD_H
