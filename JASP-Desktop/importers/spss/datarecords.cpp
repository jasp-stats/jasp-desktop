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

#include "datarecords.h"

#include "../spssimporter.h"
#include "spssimportdataset.h"
#include "../importerutils.h"
#include <cmath>

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief DataRecords ctor
 * @param fixer - Fixes byte order for data.
 * @param fileHeader the File header record.
 * @param columns The columns data we collected readling the headers.
 * @param fromStream The stream to read.
 */
DataRecords::DataRecords(SPSSImporter* importer, SPSSImportDataSet *dataset, const NumericConverter &fixer, const FileHeaderRecord &fileHeader,
						 SPSSStream &fromStream,
						 boost::function<void (const std::string &, int)> &progress)
 : _importer(importer)
 , _dataset(dataset)
 , _fileHeader(fileHeader)
 , _from(fromStream)
 , _progress(progress)
 , _fixer(fixer)
 , _numDbls(0)
 , _numStrs(0)
{
}

/**
 * @brief read Reads the values to the dataset.
 */
void DataRecords::read()
{

	if (_fileHeader.compressed() == 0)
		readUncompressed();
	else
		readCompressed();
}


/**
 * @brief readCompressed - Reads compressed data
 */
void DataRecords::readCompressed()
{
	unsigned char codes[ sizeof(Char_8) ];

	bool eofFlag = false;
	while (_from.good() && !eofFlag)
	{
		_importer->reportFileProgress(_from.tellg(), _progress);
		memset(codes, code_eof, sizeof(codes));

		_SPSSIMPORTER_READ_VAR(codes, _from);

		for (size_t cnt = 0; cnt < sizeof(codes); cnt++)
		{
			// Decode the code found.
			switch(codes[cnt])
			{
			case code_ignore: break;

			default: // A compressed data value.
				insertToCol(_importer->getNextColumn(_dataset), static_cast<double>(codes[cnt]) - _fileHeader.bias());
				break;

			case code_eof: // end of file found.
				eofFlag = true;
				break;

			case code_notCompressed:
				// Uncompressed data values follows..
				readUnCompVal(_importer->getNextColumn(_dataset));
				break;

			case code_allSpaces:
				insertToCol(_importer->getNextColumn(_dataset), string(sizeof(Char_8), ' '));
				break;

			case code_systmMissing:
				// system missing value follows.
				insertToCol(_importer->getNextColumn(_dataset), NAN);
				break;

			}
		}
	}
}


/**
 * @brief readUncompressed - Reads uncompressed data
 */
void DataRecords::readUncompressed()
{
	while (_from.good())
	{
		_importer->reportFileProgress(_from.tellg(), _progress);
		readUnCompVal(_importer->getNextColumn(_dataset));
	}
}


/**
 * @brief insertToCol Insrts a string into the (next) column.
 * @param str The string value to insert / append.
 */
void DataRecords::insertToCol(SPSSImportColumn &col, const string &str)
{
	if (col.cellType() == SPSSImportColumn::cellString)
	{
		if (_importer->isSpaning())
		{
			col.append(str);
//			DEBUG_COUT5("Appended string \"", str, "\" into column ", col.spssRawColName(), ".");
		}
		else
		{
			col.insert(str);
//			DEBUG_COUT5("Inserted string \"", str, "\" into column ", col.spssRawColName(), ".");
		}
		_numStrs++;
	}
	else
		DEBUG_COUT5("FAILED TO INSERT string \"", str, "\" into column ", col.spssRawColName(), ".");
}

/**
 * @brief insertToCol Insrts a string into the (next) column.
 * @param value The value to insert
 */
void DataRecords::insertToCol(SPSSImportColumn &col, double value)
{
	if (col.cellType() == SPSSImportColumn::cellDouble)
	{
		col.numerics.push_back(value);
		_numDbls++;
//		DEBUG_COUT5("Inserted double ", value, " into column ", col.spssRawColName(), ".");
	}
	else
		DEBUG_COUT5("FAILED TO INSERT double ", value, " into column ", col.spssRawColName(), ".");

}

/**
 * @brief readUnCompVal Reads in and stores a single data value
 * @param col the column to insert into.
 */
void DataRecords::readUnCompVal(SPSSImportColumn &col)
{
	SpssDataCell dta;
	_SPSSIMPORTER_READ_VAR(dta, _from);
	if (col.cellType() == SPSSImportColumn::cellString)
		insertToCol(col, string(dta.chars, sizeof(dta.chars)));
	else
	{
		_fixer.fixup(&dta.dbl);

		// TODO: Enstring date types!
		insertToCol(col, dta.dbl);
	}
}
