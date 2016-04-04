#include "floatinforecord.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief FloatInfoRecord Ctor
 */
FloatInfoRecord::FloatInfoRecord()
	: DataInfoRecord(FloatInfoRecord::SUB_RECORD_TYPE, FloatInfoRecord::RECORD_TYPE)
	, _sysmis( NAN )
	, _highest( DBL_MIN )
	, _lowest( DBL_MAX )
{
}

/**
 * @brief FloatInfoRecord Ctor
 * @param const HardwareFormats &fixer Fixes endainness.
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 */
FloatInfoRecord::FloatInfoRecord(const HardwareFormats &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fixer, fileSubType, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(sysmis, from, fixer);
	SPSSIMPORTER_READ_MEMBER(highest, from, fixer);
	SPSSIMPORTER_READ_MEMBER(lowest, from, fixer);
}

FloatInfoRecord::~FloatInfoRecord()
{

}

void FloatInfoRecord::process(SPSSColumns &columns)
{
	// Do Nohting.
}
