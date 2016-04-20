

#include "datarecords.h"

#include "../spssimporter.h"
#include "debug_cout.h"
#include <cmath>

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief DataRecords ctor
 * @param fileHeader the File header record.
 * @param columns The columns data we collected readling the headers.
 * @param fromStream The stream to read.
 */
DataRecords::DataRecords(const FileHeaderRecord &fileHeader, SPSSColumns &columns,
						 SPSSStream &fromStream,
						 boost::function<void (const std::string &, int)> &progress)
 : _fileHeader(fileHeader)
 , _cols(columns)
 , _from(fromStream)
 , _progress(progress)
 , _numDbls(0)
 , _numStrs(0)
{
}

/**
 * @brief read Reads the values to the dataset.
 * @param dataSet The data set to write.
 */
void DataRecords::read(/* OUT */ DataSetPackage *dataSet)
{

	if (_fileHeader.compressed() == 0)
		readUncompressed(dataSet);
	else
		readCompressed(dataSet);
}


/**
 * @brief readCompressed - Reads compressed data,
 * @param dataSet The data set to write.
 */
void DataRecords::readCompressed(/* OUT */ DataSetPackage *dataSet)
{
	unsigned char codes[ sizeof(Char_8) ];

	bool eofFlag = false;
	while (_from.good() && !eofFlag)
	{
		SPSSImporter::reportProgress(_from.tellg(), _progress);
		memset(codes, code_eof, sizeof(codes));

		_SPSSIMPORTER_READ_VAR(codes, _from);

		for (size_t cnt = 0; cnt < sizeof(codes); cnt++)
		{
			// Decode the code found.
			switch(codes[cnt])
			{
			case code_ignore: break;

			default: // A compressed data value.
				insertToCol(_cols.getNextColumn(), static_cast<double>(codes[cnt]) - _fileHeader.bias());
				break;

			case code_eof: // end of file found.
				eofFlag = true;
				break;

			case code_notCompressed:
				// Uncompressed data values follows..
				readUnCompVal(_cols.getNextColumn());
				break;

			case code_allSpaces:
				insertToCol(_cols.getNextColumn(), string(sizeof(Char_8), ' '));
				break;

			case code_systmMissing:
				// system missing value follows.
				insertToCol(_cols.getNextColumn(), NAN);
				break;

			}
		}
	}
}


/**
 * @brief readUncompressed - Reads uncompressed data,
 * @param dataSet The data set to write.
 */
void DataRecords::readUncompressed(/* OUT */ DataSetPackage *dataSet)
{
	DEBUG_COUT1("Reading UNCOMPRESSED data..");
	while (_from.good())
	{
		SPSSImporter::reportProgress(_from.tellg(), _progress);
		readUnCompVal(_cols.getNextColumn());
	}
}


/**
 * @brief insertToCol Insrts a string into the (next) column.
 * @param str The string value to insert / append.
 */
void DataRecords::insertToCol(SPSSColumn &col, const string &str)
{
	if (col.isString())
	{
		if (_cols.isSpanning())
		{
			//			col.strings[ col.strings.size() - 1 ].append(str);
			col.append(str);
//			DEBUG_COUT5("Is spanning col ", col.spssName, " index is '", col.strings().size() - 1, ".");
		}
		else
			col.insert(str);

		_numStrs++;

//		DEBUG_COUT5("Col ", col.spssName, " string is '", col.strings()[ col.strings().size() - 1 ], "'.");
	}
}

/**
 * @brief insertToCol Insrts a string into the (next) column.
 * @param value The value to insert
 */
void DataRecords::insertToCol(SPSSColumn &col, double value)
{
	if (col.isString() == false)
	{
		col.numerics.push_back(value);

		_numDbls++;
	}

//	DEBUG_COUT5("Col ", col.name, " number is : ", value, ".");
}

/**
 * @brief readUnCompVal Reads in and stores a single data value
 * @param col the column to insert into.
 */
void DataRecords::readUnCompVal(SPSSColumn &col)
{
	union u_dta { Char_8 c; double d; } dta;
	_SPSSIMPORTER_READ_VAR(dta, _from);
	if (col.isString())
		insertToCol(col, string(dta.c, col.cellCharsRemaining(sizeof(dta.c))));
	else
		insertToCol(col, dta.d);
}
