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

#include "spssimporter.h"
#include "./spss/floatinforecord.h"
#include "./spss/variablerecord.h"
#include "./spss/valuelabelvarsrecord.h"
#include "./spss/vardisplayparamrecord.h"
#include "./spss/longvarnamesrecord.h"
#include "./spss/verylongstringrecord.h"
#include "./spss/extnumbercasesrecord.h"
#include "./spss/miscinforecord.h"
#include "./spss/documentrecord.h"
#include "./spss/characterencodingrecord.h"
#include "./spss/dictionaryterminationrecord.h"
#include "./spss/datarecords.h"

#include "./convertedstringcontainer.h"

using namespace std;
using namespace boost;
using namespace spss;

SPSSImporter::SPSSImporter(DataSetPackage *packageData) : Importer(packageData)
{
	_packageData->isArchive = false;
}

SPSSImporter::~SPSSImporter()
{
}

ImportDataSet* SPSSImporter::loadFile(const string &locator, boost::function<void(const string &, int)> progress)
{
	// Open the file.
	SPSSStream stream(locator.c_str(), ios::in | ios::binary);

	// Get it's size
	stream.seekg(0, stream.end);
	_fileSize = static_cast<double>(stream.tellg());
	stream.seekg(0, stream.beg);

	// Data we have scraped to date.
	SPSSImportDataSet *dataset = new SPSSImportDataSet();

	// Fetch the dictionary.
	bool processingDict = true;
	FileHeaderRecord *pFileHeaderRecord = 0;
	IntegerInfoRecord integerInfo;
	FloatInfoRecord floatInfo;

	while(stream.good() && processingDict)
	{
		// Inform user of progress.
		reportFileProgress(stream.tellg(), progress);

		// Get the record type.
		union { int32_t u; RecordTypes t; Char_4 c; } rec_type;
		rec_type.u = rectype_unknown;
		stream.read((char *) &rec_type.u, sizeof(rec_type.u));
		// Endiness for rec_type, if known.
		if (pFileHeaderRecord != 0)
			dataset->numericsConv().fixup(&rec_type.u);

		// ... and the record type type is....
		switch(rec_type.t)
		{
		case FileHeaderRecord::RECORD_TYPE:
			pFileHeaderRecord = new FileHeaderRecord(dataset->numericsConv(), rec_type.t, stream);
			pFileHeaderRecord->process(this, dataset);
			break;

		case VariableRecord::RECORD_TYPE:
		{
			VariableRecord record(dataset->numericsConv(), rec_type.t, pFileHeaderRecord, stream);
			record.process(this, dataset);
		}
			break;

		case ValueLabelVarsRecord::RECORD_TYPE:
		{
			ValueLabelVarsRecord record(dataset->numericsConv(), rec_type.t, stream);
			record.process(this, dataset);
		}
			break;

		case rectype_meta_data: // Need to find the type of the data..
		{
			union { int32_t i; RecordSubTypes s; } sub_type;
			sub_type.s = recsubtype_unknown;
			stream.read((char *) &sub_type.i, sizeof(sub_type.i));
			dataset->numericsConv().fixup(&sub_type.i);
			switch (sub_type.s)
			{
			case  IntegerInfoRecord::SUB_RECORD_TYPE:
			{
				integerInfo = IntegerInfoRecord(dataset->numericsConv(), sub_type.s, rec_type.t, stream);
				integerInfo.process(this, dataset);
			}
				break;

			case FloatInfoRecord::SUB_RECORD_TYPE:
			{
				floatInfo = FloatInfoRecord(dataset->numericsConv(), sub_type.s, rec_type.t, stream);
				floatInfo.process(this, dataset);
			}
				break;

			case VarDisplayParamRecord::SUB_RECORD_TYPE:
			{
				VarDisplayParamRecord record(dataset->numericsConv(), sub_type.s, rec_type.t, dataset->columnCount(), stream);
				record.process(this, dataset);
			}
				break;

			case LongVarNamesRecord::SUB_RECORD_TYPE:
			{
				LongVarNamesRecord record(dataset->numericsConv(), sub_type.s, rec_type.t, stream);
				record.process(this, dataset);
			}
				break;

			case VeryLongStringRecord::SUB_RECORD_TYPE:
			{
				VeryLongStringRecord record(dataset->numericsConv(), sub_type.s, rec_type.t, stream);
				record.process(this, dataset);
			}
				break;

			case ExtNumberCasesRecord::SUB_RECORD_TYPE:
			{
				ExtNumberCasesRecord record(dataset->numericsConv(), sub_type.s, rec_type.t, stream);
				record.process(this, dataset);
			}
				break;

			case CharacterEncodingRecord::SUB_RECORD_TYPE:
			{
				CharacterEncodingRecord record(dataset->numericsConv(), sub_type.s, rec_type.t, stream);
				record.process(this, dataset);
			}
				break;

			default:
			{
				MiscInfoRecord record(dataset->numericsConv(), sub_type.i, rec_type.t, stream);
				record.process(this, dataset);
			}
			}
		}
			break;

		case DocumentRecord::RECORD_TYPE:
		{
			DocumentRecord dummy(dataset->numericsConv(), rec_type.t, stream);
			dummy.process(this, dataset);
		}
			break;

		case DictionaryTermination::RECORD_TYPE:
		{
			DictionaryTermination dummy(dataset->numericsConv(), rec_type.t, stream);
			dummy.process(this, dataset);
		}
			processingDict = false; // Got end of dictionary.
			break;

		case rectype_unknown:
		default:
		{
			string msg("Unknown record type '"); msg.append(rec_type.u, sizeof(rec_type.u)); msg.append("' found.\n"
				"The SAV importer cannot yet read this file.\n"
				"Please report this error at \n"
				"https://github.com/jasp-stats/jasp-desktop/issues\n"
				"including a small sample .SAV file that produces this message.");
			throw runtime_error(msg);
			break;
		}
		}
	}

	//If we got a file header then..
	if (pFileHeaderRecord == 0)
		throw runtime_error("No header found in .SAV file.");

	dataset->setHeaderInfo(integerInfo, floatInfo);

	// Now convert the string in the header that we are interested in.,
	ConvertedStringContainer::processAllStrings(dataset->stringsConv());

	// Set the right name for each column and build the name to col map
	dataset->setColumnMap();

	// read the data records from the file.
	DataRecords data(this, dataset, dataset->numericsConv(), *pFileHeaderRecord, stream, progress);
	data.read();

	_processStringsPostLoad(dataset, progress);


	DEBUG_COUT5("Read ", data.numDbls(), " doubles and ", data.numStrs(), " string cells.");


	// bail if unknown number of cases.
	if (dataset->hasNoCases())
		throw runtime_error("Found no cases in .SAV file.");

	delete pFileHeaderRecord;
	return dataset;
}

/**
 * @brief _processStringsPostLoad - Deals with very Long strings (len > 255) and CP processes all strings.
 * Call after the data is loaded!.
 */
void SPSSImporter::_processStringsPostLoad(SPSSImportDataSet *dataset, boost::function<void (const std::string &, int)> progress)
{
	// For every found very long string.
	const SPSSImportDataSet::LongColsData &strLens = dataset->veryLongColsDat();
	float numStrlens = distance(strLens.begin(), strLens.end());
	for (map<string, size_t>::const_iterator ituple = strLens.begin(); ituple != strLens.end(); ituple++)
	{
		{ // report progress
			float prog = 100.0 * ((float) distance(strLens.begin(), ituple)) / numStrlens;
			static float lastProg = -1.0;
			if ((prog - lastProg) >= 1.0)
			{
				progress("Processing long strings.", (int) (prog + 0.5));
				lastProg = prog;
			}

		}
		// find the root col...
		ImportColumns::iterator rootIter;
		for (rootIter = dataset->begin(); rootIter != dataset->end(); rootIter++)
		{
			SPSSImportColumn* col = dynamic_cast<SPSSImportColumn*>(*rootIter);
			DEBUG_COUT7("Matching '", ituple->first, "' against '", col->spssRawColName(), "' size ", ituple->second, ".");
			if (col->spssRawColName() == ituple->first)
					break;
		}

		const long mergedStrlen = 252;
		// Shouldn't happen..
		if (rootIter == dataset->end())
			throw runtime_error("Failed to process a very long string value.");

		SPSSImportColumn* rootColumn = dynamic_cast<SPSSImportColumn*>(*rootIter);
		// merged strings are slightly shorter than what is in the file.
		// See Appendix B System File Format, PSPP devloper's Guide, release 0.10.2 pp69
		if (rootColumn->spssStringLen() == 255)
			rootColumn->spssStringLen(mergedStrlen);

		// Chop the length of the strings in the root col.
		for (size_t cse  = 0; cse < rootColumn->strings.size(); cse++)
		{
			string str = rootColumn->strings[cse].substr(0, rootColumn->spssStringLen());
			rootColumn->strings[cse] = str;
		}

		while (rootColumn->spssStringLen() < ituple->second)
		{
			// Find the next segment, (Should be next one along)
			ImportColumns::iterator nextcolIter = rootIter;
			++nextcolIter;
			SPSSImportColumn* nextcol = dynamic_cast<SPSSImportColumn*>(*nextcolIter);
			// How much to fetch?
			long needed = min(ituple->second - rootColumn->spssStringLen(), rootColumn->spssStringLen());
			needed = min(mergedStrlen, needed);

			// Concatinate all the strings, going down the cases.
			for (size_t cse  = 0; cse < rootColumn->strings.size(); cse++)
			{
				if (needed > 0)
					rootColumn->strings[cse].append(nextcol->strings[cse], 0, needed);
			}
			// Advance the string length.
			rootColumn->spssStringLen( rootColumn->spssStringLen() + needed );
			// Dump the column.
			dataset->erase(nextcolIter);
		}
	}

	// Trim trialing spaces for all strings in the data set.
	size_t numCols = distance(dataset->begin(), dataset->end());
	for (ImportColumns::iterator iCol = dataset->begin(); iCol != dataset->end(); ++iCol)
	{
		SPSSImportColumn *col = dynamic_cast<SPSSImportColumn*>(*iCol);
		{ // report progress
			float prog = 100.0 * ((float) distance(dataset->begin(), iCol)) / numCols;
			static float lastProg = -1.0;
			if ((prog - lastProg) >= 1.0)
			{
				progress("Processing strings.", (int) (prog + 0.5));
				lastProg = prog;
			}
		}

		if (col->cellType() == SPSSImportColumn::cellString)
		{
			DEBUG_COUT3("Dumping column '", col->spssRawColName(), "'.");
			for (size_t cse  = 0; cse < col->strings.size(); cse++)
			{
				// Trim left and right.
				StrUtils::lTrimWSIP(col->strings[cse]);
				StrUtils::rTrimWSIP(col->strings[cse]);
			}
		}
	}
}

void SPSSImporter::fillSharedMemoryColumn(ImportColumn *importColumn, Column &column)
{
	SPSSImportColumn* spssCol = dynamic_cast<SPSSImportColumn*>(importColumn);

	switch(spssCol->getJaspColumnType())
	{
	default:	// Skip unknown columns
		break;
	case Column::ColumnTypeScale:
		spssCol->setColumnScaleData(column);
		break;

	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
		spssCol->setColumnAsNominalOrOrdinal(column, spssCol->getJaspColumnType());
		break;

	case Column::ColumnTypeNominalText:
		switch(spssCol->cellType())
		{
		case SPSSImportColumn::cellString:
			spssCol->setColumnConvertStringData(column);
			break;

		case SPSSImportColumn::cellDouble:
			 // convert to UTF-8 strings.
			spssCol->setColumnConvertDblToString(column);
			break;
		}
		break;
	}

}

/**
 * @brief ReportProgress Reports progress for stream.
 * @param position Position to report.
 * @param progress report to here.
 */
void SPSSImporter::reportFileProgress(SPSSStream::pos_type position, boost::function<void (const std::string &, int)> prgrss)
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

/**
 * @brief resetNextCol. reset the col iterator
 *
 */
void SPSSImporter::resetNextCol(SPSSImportDataSet *dataset)
{
	_currentColIter = dataset->begin();
	SPSSImportColumn *col = dynamic_cast<SPSSImportColumn*>(*_currentColIter);
	_remainingColSpan = col->columnSpan();
}


/**
 * @brief getNextColumn Get next column wrapping as required.
 * @return
 */
SPSSImportColumn& SPSSImporter::getNextColumn(SPSSImportDataSet *dataset)
{
	// Grab what will be the result.
	SPSSImportColumn *result = dynamic_cast<SPSSImportColumn*>(*_currentColIter);

	// Set the spaning var.
	_isSpaning = result->columnSpan() != _remainingColSpan;

	// Anthing left (for next time)?
	if (--_remainingColSpan == 0)
	{
		// Go to next column.
		++_currentColIter;
		// Dropped off end?
		if (_currentColIter == dataset->end())
		{
			resetNextCol(dataset);
		}
		else
		{
			SPSSImportColumn* col = dynamic_cast<SPSSImportColumn*>(*_currentColIter);
			_remainingColSpan = col->columnSpan();
		}
	}
	return *result;
}

