#ifndef INTEGERINFORECORD_H
#define INTEGERINFORECORD_H

#include "datainforecord.h"

namespace spss
{


/**
 * @brief The IntegerInfoRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_integer = 3,
 */
class IntegerInfoRecord : public DataInfoRecord<recsubtype_integer>
{
public:

	/**
	 * @brief IntegerInfoRecord default Ctor
	 */
	IntegerInfoRecord();

	/**
	 * @brief IntegerInfoRecord Ctor
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 */
	IntegerInfoRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &fromStream);

	virtual ~IntegerInfoRecord();

	/*
	 * Data!
	 */
	SPSSIMPORTER_READ_ATTRIB(int32_t, version_major)
	SPSSIMPORTER_READ_ATTRIB(int32_t, version_minor)
	SPSSIMPORTER_READ_ATTRIB(int32_t, version_revision)
	SPSSIMPORTER_READ_ATTRIB(int32_t, machine_code)
	SPSSIMPORTER_READ_ATTRIB(int32_t, floating_point_rep)
	SPSSIMPORTER_READ_ATTRIB(int32_t, compression_code)
	SPSSIMPORTER_READ_ATTRIB(int32_t, endianness)
	SPSSIMPORTER_READ_ATTRIB(int32_t, character_code)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);
};

}

#endif // INTEGERINFORECORD_H
