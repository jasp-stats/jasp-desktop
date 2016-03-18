#ifndef LONGVARNAMESRECORD_H
#define LONGVARNAMESRECORD_H

#include "datainforecord.h"

namespace spss
{

/**
 * @brief The LongVarNamesRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_longvar = 13,
 */
class LongVarNamesRecord : public DataInfoRecord<recsubtype_longvar>
{
public:
	/**
	 * @brief LongVarNamesRecord Ctor
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 */
	LongVarNamesRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from);

	// TODO: Break up the name / var pairs.
	// Not currently required for JASP.
	SPSSIMPORTER_READ_ATTRIB(std::string, var_name_pairs)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);
};

}

#endif // LONGVARNAMESRECORD_H
