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

#ifndef FILEHEADERRECORD_H
#define FILEHEADERRECORD_H

#include "readablerecord.h"
#include "../codepageconvert.h"

namespace spss
{

/**
 * @brief The FileHeader class: decodes file header records.
 *
 * Associated with record type rectype_file_header = 0x324C4624/"$FL2"
 */
class FileHeaderRecord : public ReadableRecord< rectype_file_header >
{
public:

	/**
	 * @brief FileHeader Read from file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream file to read.
	 * @param double expectedBias The bias value expected.
	 *
	 * An exception is thrown if the bias value read is not the
	 * same as passed - This is method used to check that the
	 * file uses the same floating point format.
	 */
	FileHeaderRecord(NumericConverter &fixer, RecordTypes fileType, SPSSStream &fromStream, double expectedBias = 100.0);

	virtual ~FileHeaderRecord();

	enum e_compressed
	{
		compression_none = 0,
		compression_bytecode = 1,
		compression_zlib = 3
	};

	/*
	 * Data elements...
	 */
	SPSSIMPORTER_READ_ATTRIB(Char_60, prod_name)
	SPSSIMPORTER_READ_ATTRIB(int32_t,layout_code)
	SPSSIMPORTER_READ_ATTRIB(int32_t, nominal_case_size)
	SPSSIMPORTER_READ_ATTRIB(int32_t, compressed)
	SPSSIMPORTER_READ_ATTRIB(int32_t, weight_index)
	SPSSIMPORTER_READ_ATTRIB(int32_t, ncases)
	SPSSIMPORTER_READ_ATTRIB(double, bias)
	SPSSIMPORTER_READ_ATTRIB(Char_9, creation_date)
	SPSSIMPORTER_READ_ATTRIB(Char_8, creation_time)
	SPSSIMPORTER_READ_ATTRIB(Char_64, file_label)
	SPSSIMPORTER_READ_ATTRIB(Char_3, padding)

	/**
	 * @brief SPSSIMPORTER_READ_ATTRIB The number of variable records found to date.
	 */
	SPSSIMPORTER_READ_ATTRIB(size_t, rawVariableCount)

	/**
	 * @brief incVarRecordCount Found another one!
	 * @return new value
	 */
	int32_t incRawVariableCount() { return _rawVariableCount++; }

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);


	/**
	 * @brief processStrings Converts any strings in the data fields.
	 * @param dictData The
	 */
	virtual void processStrings(const CodePageConvert &converter);

	/*
	 * Code Page converted values.
	 */
	SPSSIMPORTER_READ_ATTRIB(std::string, ProductName);
	SPSSIMPORTER_READ_ATTRIB(std::string, CreationDate);
	SPSSIMPORTER_READ_ATTRIB(std::string, CreationTime);
	SPSSIMPORTER_READ_ATTRIB(std::string, FileLabel);

private:
	 static const int32_t _layout_code_good_vals[3];
	 static const double _bias_good_vals[2];
};

}

#endif // FILEHEADERRECORD_H
