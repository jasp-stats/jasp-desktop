#ifndef FLOATINFORECORD_H
#define FLOATINFORECORD_H

#include "systemfileformat.h"
#include "datainforecord.h"

namespace spss {

/**
 * @brief The FloatInfoRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_float = 4,
 */
class FloatInfoRecord : public DataInfoRecord<recsubtype_float>
{
public:
	/**
	 * @brief FloatInfoRecord Ctor
	 */
	FloatInfoRecord();

	/**
	 * @brief FloatInfoRecord Ctor
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 */
	FloatInfoRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	virtual ~FloatInfoRecord();

	/*
	 * Data!
	 */
	SPSSIMPORTER_READ_ATTRIB(double, sysmis)
	SPSSIMPORTER_READ_ATTRIB(double, highest)
	SPSSIMPORTER_READ_ATTRIB(double, lowest)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);
};

}
#endif // FLOATINFORECORD_H
