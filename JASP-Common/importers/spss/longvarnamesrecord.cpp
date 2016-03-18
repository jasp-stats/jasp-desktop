#include "longvarnamesrecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief LongVarNamesRecord Ctor
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param fromStream The file to read from.
 */
LongVarNamesRecord::LongVarNamesRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fileSubType, fileType, from)
{
	char * buffer = new char[count() * size() + 2];
	from.read(buffer, count());
	_var_name_pairs.append(buffer, count());
	delete buffer;
}

/**
 * @brief process Manipulates columns by adding the contents of thie record.
 * @param columns
 *
 * Implematations should examine columns to determine the record history.
 */
void LongVarNamesRecord::process(SPSSColumns & columns)
{
	// Get the name to search for.
	map<string, string> nameVals = breakNamePairs(_var_name_pairs);

	// Find and replace the names in the columns.
	for (SPSSColumns::iterator colI = columns.begin(); colI != columns.end(); colI++)
	{
		string srchName = colI->spssName;
		// Chop trailing spaces.
		if (srchName.length() > 0)
		{
			while (srchName[srchName.length() - 1] == ' ')
				srchName = srchName.substr(0, srchName.length() - 1);
		}

		map<string, string>::const_iterator nv = nameVals.find(srchName);
		if (nv != nameVals.end())
		{
			DEBUG_COUT4("Replaced label ", colI->spssLabel, " with ", colI->spssLabel);
			colI->spssLabel = nv->second;
		}

	}
}

