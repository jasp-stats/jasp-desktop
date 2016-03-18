#include "verylongstringrecord.h"

#include "debug_cout.h"

using namespace std;
using namespace spss;


/**
 * @brief VarDisplayParamRecord Ctor
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param fromStream The file to read from.
 */
VeryLongStringRecord::VeryLongStringRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	:DataInfoRecord(fileSubType, fileType, from)
{
	// Read string lengths.
	size_t len = size() * count();
	{
		char * buffer = new char[len + 1];
		from.read(buffer, len);
		buffer[len] = '\0';
		_string_lengths = string(buffer, len);
		delete [] buffer;
	}
}


/**
 * @brief process Manipulates columns by adding the contents of thie record.
 * @param columns
 *
 * Implematations should examine columns to determine the record history.
 */
void VeryLongStringRecord::process(SPSSColumns & columns)
{
	SPSSColumns::LongColsData strLengths;

	{
		Tuples strLens = breakNamePairs(string_lengths());
		for (Tuples::const_iterator i = strLens.begin(); i != strLens.end(); i++)
			strLengths.insert(pair<string, size_t>(i->first, atol(i->second.c_str())));
	}
	columns.veryLongColsDat(strLengths);

#ifndef QT_NO_DEBUG
	DEBUG_COUT3("Found ", strLengths.size(), " tuples:");
	for (map<string, size_t>::const_iterator i = strLengths.begin(); i != strLengths.end(); i++)
	{
		DEBUG_COUT5("... name = \"", i->first, "\" value = ", i->second, ".");
	}
#endif

//	// For every found string name.
//	for (map<string, size_t>::const_iterator ituple = strLengths.begin(); ituple != strLengths.end(); ituple++)
//	{
//		// find the first col...
//		SPSSColumns::iterator icol;
//		for (icol = columns.begin(); icol != columns.end(); icol++)
//			if (icol->spssName == ituple->first) break;
//		// Shouldn't happen..
//		if (icol == columns.end()) continue;

//		// Find the total number of segements / short strings) involved.
// //		size_t numSegments = ((ituple->second + 251) / 252) * 8;
// //		DEBUG_COUT5("Found ", numSegments, " segments for very long string ", icol->spssName, ".");


//		// Find the number of chars yet to be located;
//		size_t charsToCome = ituple->second - icol->spssStringLen();
//		icol->spssStringLen(ituple->second);
//		DEBUG_COUT7("Found ", charsToCome, " chars to come, from ", ituple->second, " in vlsr, and ", icol->spssStringLen(), "in coluun.");
//		if (charsToCome > 0)
//		{
//			do
//			{
//				// Find the next segment, (Shoud be next one..)
//				SPSSColumns::iterator ncol = icol;
//				while (ncol->spssName == icol->spssName)
//					ncol++;

//				icol->columnSpan += ncol->columnSpan;
//				charsToCome = (charsToCome > ncol->spssStringLen()) ? charsToCome - ncol->spssStringLen() : 0L;
// //				numSegments--;	// One less segemnt.
//				columns.erase(ncol); // Ncol now invalid.
//			} while (charsToCome > 0);
//		}
//	}
}

