
#include "documentrecord.h"
#include "debug_cout.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief DocumentRecord Ctor
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 *
 */
DocumentRecord::DocumentRecord(RecordTypes fileType, SPSSStream &from)
	: ReadableRecord(fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(n_lines, from);

	for (int32_t i = 0; i < n_lines(); i++ )
	{
		// Read a line of 80 chars.
		string val;
		{
			char line[ LINE_LENGTH ];
			_SPSSIMPORTER_READ_VAR(line, from);
			val.append(line, LINE_LENGTH);
		}

		// Chop the right most spaces off.
		val.push_back(' ');
		size_t lastNonSpace = val.find_last_not_of(" \t\r\n");
		if (lastNonSpace != string::npos)
			val = val.substr(0, lastNonSpace + 1);
		// insert
		_lines.push_back(val);
	}
};


DocumentRecord::~DocumentRecord()
{

}

/**
 * @brief createCol Appends a colum to the vector.
 *
 */
void DocumentRecord::process(SPSSColumns & /* columns */)
{
	DEBUG_COUT1("Ignoring a found 'document record'.");
}
