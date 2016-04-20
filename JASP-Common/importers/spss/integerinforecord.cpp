
#include "integerinforecord.h"


using namespace std;
using namespace boost;
using namespace spss;


/**
 * @brief IntegerInfoRecord default Ctor
 */
IntegerInfoRecord::IntegerInfoRecord()
	: DataInfoRecord(IntegerInfoRecord::SUB_RECORD_TYPE, IntegerInfoRecord::RECORD_TYPE)
	, _version_major(0)
	, _version_minor(0)
	, _version_revision(0)
	, _machine_code(-1)		// Assume we are from spss.
	, _floating_point_rep(1)   // Assume IEEE
	, _compression_code(1)
	, _endianness(2)		   // Assume little.
	, _character_code(7)	   // 7 bit ASCII
{
}

/**
 * @brief IntegerInfoRecord Ctor
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 */
IntegerInfoRecord::IntegerInfoRecord(RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fileSubType, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(version_major, from);
	SPSSIMPORTER_READ_MEMBER(version_minor, from);
	SPSSIMPORTER_READ_MEMBER(version_revision, from);
	SPSSIMPORTER_READ_MEMBER(machine_code, from);
	SPSSIMPORTER_READ_MEMBER(floating_point_rep, from);
	SPSSIMPORTER_READ_MEMBER(compression_code, from);
	SPSSIMPORTER_READ_MEMBER(endianness, from);
	SPSSIMPORTER_READ_MEMBER(character_code, from);


	// We can only deal with IEEE Floating point numbers.
	if (floating_point_rep() != 1)
		throw runtime_error("JASP cannot read floating-point numbers from DEC or IBM mainframes - How old is this file?");
};


IntegerInfoRecord::~IntegerInfoRecord()
{

}


void IntegerInfoRecord::process(SPSSColumns &columns)
{
	// Do Nohting.
}
