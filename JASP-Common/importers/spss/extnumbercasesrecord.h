#ifndef EXTNUMBERCASESRECORD_H
#define EXTNUMBERCASESRECORD_H

#include "datainforecord.h"

namespace spss {

/**
 * @brief The ExtNumberCasesRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_extnumcases = 16,
 */
class ExtNumberCasesRecord : public DataInfoRecord<recsubtype_extnumcases>
{
public:

	/**
	 * @brief ExtNumberCasesRecord Ctor
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param from The file to read from.
	 */
	ExtNumberCasesRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	SPSSIMPORTER_READ_ATTRIB(int64_t, unknown);
	SPSSIMPORTER_READ_ATTRIB(int64_t, ncases64);

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implemetations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);
};


}

#endif // EXTNUMBERCASESRECORD_H
