
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
DocumentRecord::DocumentRecord(const NumericConverter &fixer, RecordTypes fileType, SPSSStream &from)
	: ReadableRecord(fixer, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(n_lines, from, fixer);

	for (int32_t i = 0; i < n_lines(); i++ )
	{
		// Read a line of 80 chars.
		string val;
		{
			char line[ LINE_LENGTH ];
			_SPSSIMPORTER_READ_VAR(line, from);
			val.append(line, LINE_LENGTH);
		}
	}
};


DocumentRecord::~DocumentRecord()
{

}

/**
 * @brief createCol Appends a colum to the vector.
 *
 */
void DocumentRecord::process(SPSSColumns & columns)
{
	// Chop the right most spaces off the lines.
	DEBUG_COUT1("Ignoring a found 'document record'.");
}


/**
 * @brief processStrings Converts any strings in the data fields.
 * @param dictData The
 *
 * Should be implemented in classes where holdStrings maybe or is true.
 *
 */
void DocumentRecord::processStrings(const SpssCPConvert &converter)
{
	for (size_t i = 0; i < _Lines.size(); ++i)
	{
		_Lines[i] = converter.fwdConvertCodePage( _Lines[i] );
		size_t lastNonSpace = _Lines[i].find_last_not_of(" \t\r\n");
		if (lastNonSpace != string::npos)
			_Lines[i] = _Lines[i].substr(0, lastNonSpace + 1);
	}
}
