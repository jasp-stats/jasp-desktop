//
// Copyright (C) 2015-2016 University of Amsterdam
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
#include "longvarnamesrecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief LongVarNamesRecord Ctor
 * @param const Converters &fixer The endain fixer.
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param fromStream The file to read from.
 */
LongVarNamesRecord::LongVarNamesRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fixer, fileSubType, fileType, from)
{
	char * buffer = new char[count() * size() + 2];
	from.read(buffer, count());
	_var_name_pairs.append(buffer, count());
	delete[] buffer;
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
		string srchName = colI->second.spssColumnName();
		// Chop trailing spaces.
		if (srchName.length() > 0)
		{
			while (srchName[srchName.length() - 1] == ' ')
				srchName = srchName.substr(0, srchName.length() - 1);
		}

		map<string, string>::const_iterator nv = nameVals.find(srchName);
		if (nv != nameVals.end())
		{
			string nm = columns.stringsConv().convertCodePage(nv->second);
//			DEBUG_COUT4("Replaced label ", colI->spssLabel(), " with ", nv->second);
			colI->second.spssColumnName(nm);
		}

	}
}

