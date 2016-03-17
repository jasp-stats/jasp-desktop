#ifndef VERYLONGSTRINGRECORD_H
#define VERYLONGSTRINGRECORD_H




#include "datainforecord.h"

namespace spss {

/**
 * @brief The VarDisplayParamRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type verylongstr = 14.
 */
class VeryLongStringRecord : public DataInfoRecord<recsubtype_verylongstr>
{
public:

	/**
	 * @brief VarDisplayParamRecord Ctor
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 */
	VeryLongStringRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	// The name value pairs we got.
	SPSSIMPORTER_READ_ATTRIB(std::string, string_lengths);


	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);
};

} // End namespace spss

#endif // VERYLONGSTRINGRECORD_H
