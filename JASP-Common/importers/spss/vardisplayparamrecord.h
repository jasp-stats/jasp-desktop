#ifndef VARDISPLAYPARAMRECORD_H
#define VARDISPLAYPARAMRECORD_H



#include "datainforecord.h"

namespace spss {

/**
 * @brief The VarDisplayParamRecord class
 *
 * Associated with record type rectype_value_labels = rectype_meta_data = 7, sub type recsubtype_display = 11,
 */
class VarDisplayParamRecord : public DataInfoRecord<recsubtype_display>
{
public:

	class DisplayParams
	{
	public:

		/**
		 * @brief VarDisplayRecord Ctor
		 * @param useWidth True if the width vlaue is read.
		 * @param fromStream The file to read from.
		 */
		DisplayParams(bool readWidth, SPSSStream &from);

		SPSSIMPORTER_READ_ATTRIB(int32_t, measure)
		SPSSIMPORTER_READ_ATTRIB(int32_t, width)
		SPSSIMPORTER_READ_ATTRIB(int32_t, alignment)
	};

	/**
	 * @brief VarDisplayParamRecord Ctor
	 * @param fileSubType The record subtype value, as found in the file.
	 * @param fileType The record type value, as found in the file.
	 * @param numColumns The number of columns discovered (to date)
	 * @param fromStream The file to read from.
	 */
	VarDisplayParamRecord(RecordSubTypes fileSubType, RecordTypes fileType, int32_t numCoumns, SPSSStream &from);

	std::vector<DisplayParams>  _displayParams;
	const std::vector<DisplayParams>& displayParams()
	{
		return _displayParams;
	}

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);

};

}

#endif // VARDISPLAYPARAMRECORD_H
