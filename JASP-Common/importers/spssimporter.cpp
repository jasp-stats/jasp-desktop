//
// Copyright (C) 2016 University of Amsterdam
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

#include "spssimporter.h"
#include "./spss/spssrecinter.h"
#include "./spss/floatinforecord.h"
#include "./spss/variablerecord.h"
#include "./spss/valuelabelvarsrecord.h"
#include "./spss/vardisplayparamrecord.h"
#include "./spss/longvarnamesrecord.h"
#include "./spss/verylongstringrecord.h"
#include "./spss/extnumbercasesrecord.h"
#include "./spss/miscinforecord.h"
#include "./spss/documentrecord.h"
#include "./spss/dictionaryterminationrecord.h"
#include "./spss/datarecords.h"

#include "sharedmemory.h"
#include "dataset.h"
#include "./spss/debug_cout.h"


using namespace std;

using namespace boost;
using namespace spss;

FileHeaderRecord *SPSSImporter::_pFhr = 0;
IntegerInfoRecord SPSSImporter::_integerInfo;
FloatInfoRecord SPSSImporter::_floatInfo;
double SPSSImporter::_fileSize = 0.0;



void SPSSImporter::killFhr()
{
	if (_pFhr != 0)
	{
		delete _pFhr;
		_pFhr = 0;
	}
}


void SPSSImporter::loadDataSet(
		DataSetPackage *packageData,
		const std::string &locator,
		boost::function<void (const std::string &, int)> progress)
{
	(void)progress;
	packageData->isArchive = false;						 // SPSS/spss files are never archives.
	packageData->dataSet = SharedMemory::createDataSet();   // Do our space.

	killFhr();

	// Open the file.
	SPSSStream stream(locator.c_str(), ios::in | ios::binary);

	// Get it's size
	stream.seekg(0, stream.end);
	_fileSize = static_cast<double>(stream.tellg());
	stream.seekg(0, stream.beg);

	// Data we have scraped to date.
	SPSSColumns dictData;

	// Fetch the dictionary.
	bool processingDict = true;

	while(stream.good() && processingDict)
	{
		reportProgress(stream.tellg(), progress);
		union { int32_t u; RecordTypes t; Char_4 c; } rec_type;
		rec_type.u = rectype_unknown;
		stream.read((char *) &rec_type.u, sizeof(rec_type.u));
		switch(rec_type.t)
		{
		case FileHeaderRecord::RECORD_TYPE:
			_pFhr = new FileHeaderRecord(rec_type.t, stream);
			_pFhr->process(dictData);
			break;

		case VariableRecord::RECORD_TYPE:
		{
			VariableRecord record(rec_type.t, _pFhr, stream);
			record.process(dictData);
		}
			break;

		case ValueLabelVarsRecord::RECORD_TYPE:
		{
			ValueLabelVarsRecord record(rec_type.t, stream);
			record.process(dictData);
		}
			break;

		case rectype_meta_data: // Need to find the type of the data..
		{
            union { int32_t i; RecordSubTypes s; } sub_type;
			sub_type.s = recsubtype_unknown;
			stream.read((char *) &sub_type.i, sizeof(sub_type.i));
			switch (sub_type.s)
			{
			case  IntegerInfoRecord::SUB_RECORD_TYPE:
			{
				_integerInfo = IntegerInfoRecord(sub_type.s, rec_type.t, stream);
				_integerInfo.process(dictData);
			}
				break;

			case FloatInfoRecord::SUB_RECORD_TYPE:
			{
				_floatInfo = FloatInfoRecord(sub_type.s, rec_type.t, stream);
				_floatInfo.process(dictData);
			}
				break;

			case VarDisplayParamRecord::SUB_RECORD_TYPE:
			{
				VarDisplayParamRecord record(sub_type.s, rec_type.t, dictData.size(), stream);
				record.process(dictData);
			}
				break;

			case LongVarNamesRecord::SUB_RECORD_TYPE:
			{
				LongVarNamesRecord record(sub_type.s, rec_type.t, stream);
				record.process(dictData);
			}
				break;

			case VeryLongStringRecord::SUB_RECORD_TYPE:
			{
				VeryLongStringRecord record(sub_type.s, rec_type.t, stream);
				record.process(dictData);
			}
				break;

			case ExtNumberCasesRecord::SUB_RECORD_TYPE:
			{
				ExtNumberCasesRecord record(sub_type.s, rec_type.t, stream);
				record.process(dictData);
			}
				break;

			default:
			{
				MiscInfoRecord record(sub_type.i, rec_type.t, stream);
				record.process(dictData);
			}
			}
		}
			break;

		case DocumentRecord::RECORD_TYPE:
		{
			DocumentRecord dummy(rec_type.t, stream);
			dummy.process(dictData);
		}
			break;

		case DictionaryTermination::RECORD_TYPE:
		{
			DictionaryTermination dummy(rec_type.t, stream);
			dummy.process(dictData);
		}
			processingDict = false; // Got end of dictionary.
			break;

		case rectype_unknown:
		default:
        {
            string msg("Unknown record type '");
            msg.append(rec_type.c, sizeof(rec_type.c));
            msg.append("' found.\nThe SAV importer cannot read this file.");
            throw runtime_error(msg);
			break;
		}
        }
	}

	//If we got a file header then..
	if (_pFhr == 0)
		throw runtime_error("No header found in .SAV file.");

	// read the data records from the file.
	DataRecords data(*_pFhr, dictData, stream, progress);
	data.read(packageData);

	dictData.processVeryLongStrings();

	DEBUG_COUT5("Read ", data.numDbls(), " doubles and ", data.numStrs(), " string cells.");


	// bail if unknown number of cases.
	if (dictData.hasNoCases())
		throw runtime_error("Found no cases in .SAV file.");

	// Set the data size
	setDataSetSize(*packageData, dictData.numCases(), dictData.size());

	// Now go fetch the data.
	for (size_t i = 0; i < dictData.size(); i++)
	{
		SPSSColumn &spss = dictData[i];
		Column &column = packageData->dataSet->column(i);

		column.setName(spss.spssLabel);
		column.setColumnType(convert(spss.measure));
		column.labels().clear();

		{
			switch(column.columnType())
			{
			case Column::ColumnTypeScale:
			{
				Column::Doubles::iterator doubleInputItr = column.AsDoubles.begin();
				for (size_t row = 0; (row < dictData.numCases()) && (doubleInputItr != column.AsDoubles.end()) ; row++, doubleInputItr++)
				{
					double val = spss.numerics[row];
					// Jasp uses a NAN as missing value.
					*doubleInputItr = spss.missingChecker.processMissingValue(_floatInfo, val);
				}
			}
				break;
			case Column::ColumnTypeNominal:
			case Column::ColumnTypeOrdinal:
			{
				Column::Ints::iterator intsInputItr = column.AsInts.begin();
				for (size_t row = 0; (row < dictData.numCases()) && (intsInputItr != column.AsInts.end()) ; row++, intsInputItr++)
					*intsInputItr = static_cast<int>(spss.numerics[row]);
			}
				break;
			case Column::ColumnTypeNominalText:
			{
				const vector<string> &strs = spss.strings();

				map<int, int> indexes; // index keyed by row.
				{
					set<string> unique;
					for (size_t i = 0; i < strs.size(); i++)
					{
						if (strs[i].size() != 0)
							unique.insert(strs[i]);
					}

#ifndef QT_NO_DEBUG
					DEBUG_COUT3("Strings ex unique (N=", unique.size(), ")");
					for (set<string>::const_iterator  i = unique.begin(); i != unique.end(); i++)
					{
						DEBUG_COUT3("... \"", *i, "\".");
					}
#endif
					assert(column.labels().size() == 0);
					for (set<string>::const_iterator i = unique.begin(); i != unique.end(); i++)
						column.labels().add(*i);

					// Build the indexes to the lables.
					for (size_t row = 0; row < dictData.numCases(); row++)
					{
						set<string>::const_iterator is = unique.find(strs[row]);
						if ((is == unique.end()) || (is->length() == 0) || (*is == "") || (*is == " "))
							indexes.insert( pair<int, int>(row, INT_MIN) );
						else
							indexes.insert( pair<int, int>(row, distance(unique.begin(), is)) );
					}

#ifndef QT_NO_DEBUG
					DEBUG_COUT3("Indexes (N=", indexes.size(), ")");
					for (map<int, int>::const_iterator  i = indexes.begin(); i != indexes.end(); i++)
					{
						DEBUG_COUT5("... row=", i->first, " index=", i->second, ".");
					}
#endif
				} // Done with unique.


				Column::Ints::iterator intsInputItr = column.AsInts.begin();
				for (size_t row = 0; (row < dictData.numCases()) && (intsInputItr != column.AsInts.end()); row++, intsInputItr++)
				{
					map<int, int>::iterator indexI = indexes.find(row);
					assert(indexI != indexes.end());
					*intsInputItr = indexI->second;
#ifndef QT_NO_DEBUG
					DEBUG_COUT5("Inserted index :", *intsInputItr, " for row :", row, ".");
#endif
				}
			}
				break;
			}
		}
	}

	if (stream.bad())
	{
		killFhr();
		SharedMemory::deleteDataSet(packageData->dataSet);
		throw runtime_error("Error reading .SAV file.");
	}
};


/**
 * @brief setDataSetSize Sets the data set size in the passed
 * @param dataSet The Data set to manipluate.
 * @param rowCount The (real) number of rows we have.
 * @param colCount The number of columns.
 */
void SPSSImporter::setDataSetSize(DataSetPackage &dataSetPg, size_t rowCount, size_t colCount)
{
	bool retry(true);
	do
	{
		try {

			dataSetPg.dataSet->setColumnCount(colCount);
			dataSetPg.dataSet->setRowCount(rowCount);
			retry = false;
		}
		catch (boost::interprocess::bad_alloc &e)
		{

			try {

				dataSetPg.dataSet = SharedMemory::enlargeDataSet(dataSetPg.dataSet);
				retry = false;
			}
			catch (std::exception &e)
			{
				DEBUG_COUT2("SPSSImporter::setDataSetSize(), reallocating. std::exeption: ", e.what());
				throw runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (std::exception e)
		{
			DEBUG_COUT2("SPSSImporter::setDataSetSize() std::exeption: ", e.what());
			throw runtime_error("Exception when reading .SAV file.");
		}
		catch (...)
		{
			DEBUG_COUT1("SPSSImporter::setDataSetSize() unknown exception.");
			throw runtime_error("Unidentified error when reading .SAV file.");
		}
	}
	while (retry);


}


/**
 * @brief convert convert from spss/SPSS measure value to JASP.
 * @param measure spss value.
 * @return Converted measure
 */
Column::ColumnType SPSSImporter::convert(int32_t measure)
{
	switch(measure)
	{
	case Measures::measure_continuous:
	case Measures::measure_spss_unknown:
	case Measures::measure_undefined:
		return Column::ColumnTypeScale;
	case Measures::measure_nominal: return Column::ColumnTypeNominal;
	case Measures::measure_ordinal: return Column::ColumnTypeOrdinal;
	case Measures::string_type: return Column::ColumnTypeNominalText;

	}
	return Column::ColumnTypeScale;
}


/**
 * @brief ReportProgress Reports progress for stream.
 * @param position Position to report.
 * @param progress report to here.
 */
void SPSSImporter::reportProgress(SPSSStream::pos_type position, boost::function<void (const std::string &, int)> prgrss)
{
	static int lastPC = -1.0;
	int thisPC = static_cast<int>((100.0 * static_cast<double>(position) / _fileSize) + 0.5);
	if (lastPC != thisPC)
	{
		int pg = static_cast<int>(thisPC + 0.5);
		prgrss("Loading .SAV file", pg);
		lastPC = thisPC;
	}
}

