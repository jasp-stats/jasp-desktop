


#include "extnumbercasesrecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief ExtNumberCasesRecord Ctor
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 */
ExtNumberCasesRecord::ExtNumberCasesRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fileSubType, fileType, from)
{
	// Go through the fields, just fetching as we go..
	SPSSIMPORTER_READ_MEMBER(unknown, from);
	SPSSIMPORTER_READ_MEMBER(ncases64, from);
};


void ExtNumberCasesRecord::process(SPSSColumns &columns)
{

	// Extract the number of cases.
	if (ncases64() != -1L)
		columns.numCases(ncases64());
}
