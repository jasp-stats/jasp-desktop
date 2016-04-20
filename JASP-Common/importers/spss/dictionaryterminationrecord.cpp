
#include "dictionaryterminationrecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief DictionaryTermination Ctor
 * @param fileType The record type value, as found in the file.
 * @param fromStream File to read from
 */
DictionaryTermination::DictionaryTermination(RecordTypes fileType, SPSSStream &from)
 : ReadableRecord(fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(filler, from);
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
