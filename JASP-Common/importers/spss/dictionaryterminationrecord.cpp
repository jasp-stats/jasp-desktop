
#include "dictionaryterminationrecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief DictionaryTermination Ctor
 * @param fileType The record type value, as found in the file.
 * @param fromStream File to read from
 */
DictionaryTermination::DictionaryTermination(const HardwareFormats &fixer, RecordTypes fileType, SPSSStream &from)
 : ReadableRecord(fixer, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(filler, from, fixer);
}

DictionaryTermination::~DictionaryTermination()
{

}

/**
 * @brief Does nothing
 *
 */
void DictionaryTermination::process(SPSSColumns &columns)
{
	// inin the columns iterator.
	columns.resetCols();
}
