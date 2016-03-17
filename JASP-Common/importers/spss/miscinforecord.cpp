

#include "miscinforecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief MiscInfoRecord::MiscInfoRecord
 * @param fileSubType the record sub type from file.
 * @param fileType The record type from file
 * @param fromStream The file to read.
 */
MiscInfoRecord::MiscInfoRecord(int32_t fileSubType, RecordTypes fileType, SPSSStream &from)
	: ReadableRecord(fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(size, from);
	SPSSIMPORTER_READ_MEMBER(count, from);
	{
		size_t sizeData = _size * _count;
		char * buffer = new char[sizeData + 2];
		from.read(buffer, sizeData);
		_data.append(buffer, sizeData);
		delete buffer;
	}
}


/**
 * @brief process Manipulates columns by adding the contents of thie record.
 * @param columns
 *
 * Implematations should examine columns to determine the record history.
 */
void MiscInfoRecord::process(SPSSColumns & /* columns */)
{

}
