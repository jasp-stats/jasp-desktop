

#include "vardisplayparamrecord.h"
#include "debug_cout.h"

using namespace std;
using namespace boost;
using namespace spss;


/**
 * @brief VarDisplayParamRecord Ctor
 * @param useWidth True if we use the width.
 * @param fromStream The file to read from.
 */
VarDisplayParamRecord::DisplayParams::DisplayParams(bool usedWidth, SPSSStream &from)
 : _width(0)
{
	SPSSIMPORTER_READ_MEMBER(measure, from);
	if (usedWidth)
	{
		SPSSIMPORTER_READ_MEMBER(width, from);
	}
	SPSSIMPORTER_READ_MEMBER(alignment, from);
};



/**
 * @brief VarDisplayRecord Ctor
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param numCoumns The number of columns discovered (to date)
 * @param fromStream The file to read from.
 */
VarDisplayParamRecord::VarDisplayParamRecord(RecordSubTypes fileSubType, RecordTypes fileType, int32_t numColumns, SPSSStream &from)
	: DataInfoRecord(fileSubType, fileType, from)
{
	// do we read the width value?
	bool useWidth = count() > (2 * numColumns);

	// Fetch all the records.
	for (int i = 0; i < numColumns; i++)
	{
		DisplayParams dp(useWidth, from);
		_displayParams.push_back( dp );
	}
}


/**
 * @brief process Manipulates columns by adding the contents of thie record.
 * @param columns
 *
 * Implematations should examine columns to determine the record history.
 */
void VarDisplayParamRecord::process(SPSSColumns &columns)
{
	// String continuation columns do not have a display params entry.
	for (size_t colCount = 0; colCount < columns.size(); ++colCount)
	{
		// place measure in this col.
		if (columns[colCount].measure != Measures::string_type)
			columns[colCount].measure = _displayParams[colCount].measure();

		DEBUG_COUT6("Measure for col :\"", columns[colCount].spssName, "\" (", columns[colCount].spssName, "\") set to ", columns[colCount].measure);
	}
}
