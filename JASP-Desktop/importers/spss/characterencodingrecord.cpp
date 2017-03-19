//
// Copyright (C) 2017 University of Amsterdam
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

#include "characterencodingrecord.h"
#include "spssimportdataset.h"

using namespace std;
using namespace spss;

/**< character set name substitution. */
CharacterEncodingRecord::_NameSubs CharacterEncodingRecord::_nameSubstitution;


/**
 * @brief CharacterEncodingRecord Ctor
 * @param Converters fixer Fixes endainness.
 * @param fileSubType The record subtype value, as found in the file.
 * @param fileType The record type value, as found in the file.
 * @param fromStream The file to read from.
 */
CharacterEncodingRecord::CharacterEncodingRecord(const NumericConverter &fixer, RecordSubTypes fileSubType, RecordTypes fileType, SPSSStream &from)
 : DataInfoRecord(CharacterEncodingRecord::SUB_RECORD_TYPE, CharacterEncodingRecord::RECORD_TYPE)
 , _size(0)
 , _count(0)
{
	SPSSIMPORTER_READ_MEMBER(size, from, fixer);
	SPSSIMPORTER_READ_MEMBER(count, from, fixer);
	{
		size_t numBytes = count() * size();
		char buffer[numBytes+1];
		from.read(buffer, numBytes);
		_encoding = string(buffer, numBytes);
	}

	if (_nameSubstitution.size() == 0)
		_nameSubstitution = _buildNs();
}

CharacterEncodingRecord::~CharacterEncodingRecord()
{

}


/**
 * @brief process Manipulates columns by adding the contents of thie record.
 *
 */
void CharacterEncodingRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	string buffer = encoding();
	// This should work, since we are only really expecting ASCII
	transform(buffer.begin(), buffer.end(), buffer.begin(), ::tolower);
	_NameSubs::const_iterator i = _nameSubstitution.find(buffer);
	if  (i != _nameSubstitution.end())
		buffer = i->second;

	dataset->setStrCnvrtr(new CodePageConvert( buffer.c_str() ));
}

/**
 *  @brief _buildNs Builds the nameSubstitution map.
 *  @return
 *
 */
CharacterEncodingRecord::_NameSubs CharacterEncodingRecord::_buildNs()
{
	map<string, string> result;

	result.insert(pair<string, string>("us-ascii", "utf-8"));

	return result;
}
