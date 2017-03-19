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

#include "spssimportdataset.h"
#include "variablerecord.h"
#include "../importerutils.h"
#include "stringutils.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief VariableRecord Ctor
 * @param fixer - The Endian fixer.
 * @param fileType The record type value, as found in the file.
 * @param fileHeader The file ehadewr we are associated with.
 * @param fromStream The file to read from.
 *
 */
VariableRecord::VariableRecord(const NumericConverter &fixer, RecordTypes fileType, FileHeaderRecord * fileHeader, SPSSStream &from)
	: ReadableRecord(fixer, fileType, from)
{
	/*
	 * The data,
	 */
	SPSSIMPORTER_READ_MEMBER(type, from, fixer);
	SPSSIMPORTER_READ_MEMBER(has_var_label, from, fixer);
	SPSSIMPORTER_READ_MEMBER(n_missing_values, from, fixer);
	SPSSIMPORTER_READ_MEMBER(print, from, fixer);
	SPSSIMPORTER_READ_MEMBER(write, from, fixer);
	SPSSIMPORTER_READ_MEMBER(nameInFile, from, fixer);

	{
		const size_t numChars = sizeof(_nameInFile) / sizeof(char);
		char buffer[numChars + 1];
		memcpy(buffer, _nameInFile, numChars);
		buffer[numChars] = '\0';
		StrUtils::rTrimWSIP(buffer, numChars - 1);
		_name = buffer;
	}

	if (has_var_label() != 0)
	{
		SPSSIMPORTER_READ_MEMBER(label_len, from, fixer);
		if (label_len() > 0)
		{
			// Find the buffer size rounded up to 32 bit increments,
			size_t buffSize = roundUpTo(label_len(), 32);
			char * buffer = new char[ buffSize ];
			from.read(buffer, buffSize);
			_label.append(buffer, label_len());
			delete[] buffer;
		}
	}

	for (int32_t i = 0; i != abs(n_missing_values()); i++)
	{
		double val;
		from.read((char *) &val, sizeof(val)/sizeof(char));
		_missing_values.push_back(val);
	}

	_dictIndex = fileHeader->incRawVariableCount();
}


VariableRecord::~VariableRecord()
{

}

/**
 * @brief createCol Appends a colum to the vector.
 *
 */
void VariableRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{

	// check for string continuation.
	if (isStringContinuation())
	{
		ImportColumns::reverse_iterator iter = dataset->rbegin();
		if (iter == dataset->rend())
			throw runtime_error("Programming error: invalid last column found in VariableRecord::process.");

		SPSSImportColumn* col = dynamic_cast<SPSSImportColumn*>(*iter);
		if (col->cellType() == SPSSImportColumn::cellString)
			col->incrementColumnSpan();

//		DEBUG_COUT5("Existing column ", columns[columns.size()-1].spssName(), " spans ", columns[columns.size()-1].columnSpan(), " cols.");

		return;
	}

	SPSSImportColumn *col = new SPSSImportColumn(dataset, name(), label(), type(), _getType(print()), MissingValueChecker(n_missing_values(), missing_values()));
	dataset->add(dictIndex(), col);
}
