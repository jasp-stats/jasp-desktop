/*
 * Define the file header record,
 */

#include "fileheaderrecord.h"

#include <assert.h>

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief FileHeader Read from file.
 * @param fileType The record type value, as found in the file.
 * @param from file to read.
 * @param double expectedBias The bias value expected.
 *
 * An exception is thrown if the bias value read is not the
 * same as passed - This is method used to check that the
 * file uses the same floating point format.
 */
FileHeaderRecord::FileHeaderRecord(RecordTypes fileType, SPSSStream &from, double expectedBias)
	: ReadableRecord(fileType, from)
	, _varRecordCount(0)
{
	// Go through the fields, just fetching as we go..
	SPSSIMPORTER_READ_MEMBER(prod_name, from);
	SPSSIMPORTER_READ_MEMBER(layout_code, from);
	SPSSIMPORTER_READ_MEMBER(nominal_case_size, from);
	SPSSIMPORTER_READ_MEMBER(compressed, from);
	SPSSIMPORTER_READ_MEMBER(weight_index, from);
	SPSSIMPORTER_READ_MEMBER(ncases, from);
	SPSSIMPORTER_READ_MEMBER(bias, from);
	SPSSIMPORTER_READ_MEMBER(creation_date, from);
	SPSSIMPORTER_READ_MEMBER(creation_time, from);
	SPSSIMPORTER_READ_MEMBER(file_label, from);
	SPSSIMPORTER_READ_MEMBER(padding, from);

	if (biasIsAcceptable(expectedBias) == false)
	{
		DEBUG_COUT5("FileHeaderRecord()::ctor Got bias value ", bias(), " expected ", expectedBias, ".");
		throw runtime_error("Bias value unexpected - cannot read floating point numbers!");
	}

#ifndef QT_NO_DEBUG
	cout << "File is ";
	switch(compressed())
	{
	case compression_none:	  cout << "not compressed";   break;
	case compression_bytecode:  cout << "byte coded";	   break;
	case compression_zlib:	  cout << "ZLIB compressed";  break;
	default:					cout << "broken!";		  break;
	}
	cout << endl;
	cout.flush();
#endif
	switch(compressed())
	{
	case compression_none:
	case compression_bytecode:
	case compression_zlib:
		break;
	default:
		throw runtime_error("Cannot find compression type for .SAV file.");
	}
}

FileHeaderRecord::~FileHeaderRecord()
{

}

/**
 * @brief Clears the columns.
 *
 */
void FileHeaderRecord::process(SPSSColumns &columns)
{
	if (columns.size() != 0)
	{
		cout << "This file appears to have more than one file header record.\n"
				"  Only the last one found will be used." << endl;
		cout.flush();
	}
	columns.clear();

	// Extract the number of cases.
	if (ncases() != -1)
		columns.numCases(ncases());
}
