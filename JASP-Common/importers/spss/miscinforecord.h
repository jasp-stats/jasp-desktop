#ifndef MISCINFORECORD_H
#define MISCINFORECORD_H

#include "datainforecord.h"

namespace spss
{

/**
 * @brief The DataInfoRecordVarSub class
 *
 * Handles info record of sub type "other".
 */
class MiscInfoRecord : public ReadableRecord<rectype_meta_data>
{
public:

	/**
	 * @brief MiscInfoRecord::MiscInfoRecord
	 * @param fileSubType the record sub type from file.
	 * @param fileType The record type from file
	 * @param fromStream The file to read.
	 */
	MiscInfoRecord(int32_t fileSubType, RecordTypes fileType, SPSSStream &fromStream);

	SPSSIMPORTER_READ_ATTRIB(int32_t, size)
	SPSSIMPORTER_READ_ATTRIB(int32_t, count)
	SPSSIMPORTER_READ_ATTRIB(std::string, data);


	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);
};

}

#endif // MISCINFORECORD_H
