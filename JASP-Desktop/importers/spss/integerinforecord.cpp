//
// Copyright (C) 2015-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "integerinforecord.h"
#include "spssimportdataset.h"


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
 * @param fixer The endiness fixer
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 */
IntegerInfoRecord::IntegerInfoRecord(NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
	: DataInfoRecord(fixer, fileSubType, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(version_major, from, fixer);
	SPSSIMPORTER_READ_MEMBER(version_minor, from, fixer);
	SPSSIMPORTER_READ_MEMBER(version_revision, from, fixer);
	SPSSIMPORTER_READ_MEMBER(machine_code, from, fixer);
	SPSSIMPORTER_READ_MEMBER(floating_point_rep, from, fixer);
	// Decode the floating point info.
	{
		NumericConverter::FPTypes fpType = NumericConverter::fp_unknown;
		switch(floating_point_rep())
		{
		case 1:
			fpType = NumericConverter::fp_IEEE754;
			break;
		case 2:
			fpType = NumericConverter::fp_IBM370;
			break;
		case 3:
			fpType = NumericConverter::fp_DECVAX_E;
			break;
		}
		fixer.setFPType(fpType);
	}

	SPSSIMPORTER_READ_MEMBER(compression_code, from, fixer);
	SPSSIMPORTER_READ_MEMBER(endianness, from, fixer);
	// Decode the endiannses.
	{
		NumericConverter::Endians endian = NumericConverter::mach_unknown;
		switch(endianness())
		{
		case 1:
			endian = NumericConverter::mach_bigEndian;
			break;
		case 2:
			endian = NumericConverter::mach_littleEndian;
			break;
		}
		fixer.setEndian(endian);
	}
	SPSSIMPORTER_READ_MEMBER(character_code, from, fixer);
};


IntegerInfoRecord::~IntegerInfoRecord()
{
}


void IntegerInfoRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	// Build a string convertor based on the charactor code.
	dataset->setStrCnvrtr( new CodePageConvert(CodePageConvert::findIANANameFromSPSS(character_code())));
}
