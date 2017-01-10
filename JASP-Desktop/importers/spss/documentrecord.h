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

#ifndef DOCUMENTRECORD_H
#define DOCUMENTRECORD_H

#include "systemfileformat.h"
#include "readablerecord.h"
namespace spss
{

/**
 * @brief The DocumentRecord class : decodes documents
 *
 * Associated with record type rectype_document = 6
 */
class DocumentRecord : public ReadableRecord<rectype_document>
{
public:

	/**
	 * @brief DocumentRecord Ctor
	 * @param const Converters &fixer Fixes endiness.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 *
	 */
	DocumentRecord(const NumericConverter &fixer, RecordTypes fileType, SPSSStream &fromStream);

	virtual ~DocumentRecord();

	SPSSIMPORTER_READ_ATTRIB(int32_t,					n_lines)
	SPSSIMPORTER_READ_ATTRIB(std::vector< std::string >, Lines)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);


	static const size_t LINE_LENGTH = 80;
};

}

#endif // DOCUMENTRECORD_H
