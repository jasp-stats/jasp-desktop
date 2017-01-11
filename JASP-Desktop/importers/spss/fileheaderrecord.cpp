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

/*
 * Define the file header record,
 */

#include "fileheaderrecord.h"
#include "spssimportdataset.h"

#include <assert.h>

using namespace std;
using namespace boost;
using namespace spss;

const int32_t FileHeaderRecord::_layout_code_good_vals[3] = {2, 3, 0};
const double FileHeaderRecord::_bias_good_vals[2] = { 100.0, 0.0 };

/**
 * @brief FileHeader Read from file.
 * @param Converters &fixer - Fixer for eindiness.
 * @param fileType The record type value, as found in the file.
 * @param from file to read.
 * @param double expectedBias The bias value expected.
 *
 * An exception is thrown if the bias value read is not the
 * same as passed - This is method used to check that the
 * file uses the same floating point format.
 */
FileHeaderRecord::FileHeaderRecord(NumericConverter &fixer, RecordTypes fileType, SPSSStream &from, double expectedBias)
	: ReadableRecord(fixer, fileType, from)
	, _rawVariableCount(0)
{
	// Go through the fields, just fetching as we go..
	_SPSSIMPORTER_READ_VAR(_prod_name, from);
	_SPSSIMPORTER_READ_VAR(_layout_code, from);
	// NEW Endinness check, at the start of the thing.
	fixer.analyse(_layout_code, _layout_code_good_vals);
	SPSSIMPORTER_READ_MEMBER(nominal_case_size, from, fixer);
	SPSSIMPORTER_READ_MEMBER(compressed, from, fixer);
	SPSSIMPORTER_READ_MEMBER(weight_index, from, fixer);
	SPSSIMPORTER_READ_MEMBER(ncases, from, fixer);
	_SPSSIMPORTER_READ_VAR(_bias, from);
	// Attempt to check the bias value,
	// assuming it is the same endian as ints,
	// if value is not 100.
	try { fixer.analyse(_bias, _bias_good_vals); }
	catch (runtime_error &e) { fixer.forceLocalSystemDbl(); }
	fixer.fixup(&_bias);
	SPSSIMPORTER_READ_MEMBER(creation_date, from, fixer);
	SPSSIMPORTER_READ_MEMBER(creation_time, from, fixer);
	SPSSIMPORTER_READ_MEMBER(file_label, from, fixer);
	SPSSIMPORTER_READ_MEMBER(padding, from, fixer);
}

FileHeaderRecord::~FileHeaderRecord()
{
}

/**
 * @brief Clears the columns.
 *
 */
void FileHeaderRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	if (dataset->columnCount() != 0)
	{
		cout << "This file appears to have more than one file header record.\n"
				"  Only the last one found will be used." << endl;
		cout.flush();
		dataset->clear();
	}

	// Check compression type.
	switch(compressed())
	{
	case compression_none:
	case compression_bytecode:
	case compression_zlib:
		break;
	default:
		throw runtime_error("Cannot find compression type for .SAV file.");
	}

	// Extract the number of cases.
	if (ncases() != -1)
		dataset->numCases(ncases());
}

/**
 * @brief processStrings Converts any strings in the data fields.
 * @param dictData The
 */
void FileHeaderRecord::processStrings(const CodePageConvert &converter)
{
	_ProductName = converter.convertCodePage( _prod_name, sizeof(_prod_name) );
	_CreationDate = converter.convertCodePage( _creation_date, sizeof(_creation_date) );
	_CreationTime = converter.convertCodePage( _creation_time, sizeof(_creation_time) );
	_FileLabel = converter.convertCodePage( _file_label, sizeof(_file_label) );
}
