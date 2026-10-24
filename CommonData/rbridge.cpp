//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
#include "rbridge.h"
#include <json/json.h>
#include "dataset.h"
#include "appinfo.h"
#include "tempfiles.h"
#include "log.h"
#include "timers.h"
#include "r_functionwhitelist.h"
#include "otoolstuff.h"
#include "databridge.h"
#include "r_functionwhitelist.h"
#include <sstream>
#include <cassert>


#ifdef _WIN32
#include <windows.h>
#endif

DataBridge					*	data_bridge				= nullptr;
DataSet						*	rbridge_dataSet			= nullptr;
RCallback						rbridge_callback		= NULL;
std::set<std::string>			filterColumnsUsed;
std::vector<std::string>		columnNamesInDataSet;
ColumnEncoder				*	extraEncodings			= nullptr;
ColumnEncoder::colsPlusTypes	datasetWanted;
std::string						computedColumnFilter	= "";

char** rbridge_getLabels(const Labels &levels, size_t &nbLevels);
char** rbridge_getLabels(const std::vector<std::string> &levels, size_t &nbLevels);

size_t _logWriteFunction(const void * buf, size_t len)
{
	try 
	{
		if(len > 0 && buf)
			Log::log(false).write(static_cast<const char *>(buf), len) << std::flush;
	} 
	catch (...) 
	{
		Log::log() << "there was a problem writing to buffer from R" << std::endl;
	}
	
	return len;
}

void rbridge_setDataBridge(DataBridge * dataBridge)
{
	data_bridge = dataBridge;
	rbridge_dataSet = nullptr;
	extraEncodings = dataBridge ? dataBridge->extraEncodings() : nullptr;
}

void rbridge_clearDataBridge()
{
	rbridge_setDataBridge(nullptr);
}

const std::string jaspBaseDistributionSamplersR =
		#include "jaspBase_distributionSamplers.h"
		;

const std::string jaspBaseFriendlyConstructorFunctionsR =
		#include "jaspBase_friendlyConstructorFunctions.h"
		;

const std::string jaspBaseTransformFunctionsR =
		#include "jaspBase_transformFunctions.h"
		;


const std::string jaspBaseTransformBoxCoxR =
		#include "jaspBase_transformBoxCox.h"
		;

const std::string jaspBaseTransformJohnsonR =
		#include "jaspBase_transformJohnson.h"
		;

const std::string jaspBaseTransformYeoJohnsonR =
		#include "jaspBase_transformYeoJohnson.h"
		;

const std::string jaspBaseTransformPowerR =
		#include "jaspBase_transformPower.h"
		;

void rbridge_init(DataBridge * dataBridge, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction, const char * resultFont, bool insideJasp)
{
	JASPTIMER_SCOPE(rbridge_init);

	Log::log() << "Setting DataBridge and extraEncodings." << std::endl;
	rbridge_setDataBridge(dataBridge);

	Log::log() << "Collecting RBridgeCallBacks." << std::endl;
	RBridgeCallBacks callbacks = {
		rbridge_readDataSet,
		rbridge_readDataSetRequested,
		rbridge_readDataColumnNames,
		rbridge_readDataSetDescription,
		rbridge_requestStateFileSource,
		rbridge_requestTempFileName,
		rbridge_requestSpecificFileName,
		rbridge_requestTempRootName,
		rbridge_runCallback,
		rbridge_readFullDataSet,
		rbridge_readFullFilteredDataSet,
		rbridge_readDataSetForFiltering,
		rbridge_readDataSetForComputedColumns,
		rbridge_requestJaspResultsFileSource,
		rbridge_getColumnType,
		rbridge_createColumn,
		rbridge_deleteColumn,
		rbridge_getColumnAnalysisId,
		rbridge_getColumnOriginalIndex,
		rbridge_setColumnDataAndType,
		rbridge_dataSetRowCount,
		rbridge_encodeColumnName,
		rbridge_decodeColumnName,
		rbridge_encodeAllColumnNames,
		rbridge_decodeAllColumnNames,
		rbridge_decodeColumnType,
		rbridge_shouldEncodeColumnName,
		rbridge_shouldDecodeColumnName,
		rbridge_allColumnNames,
		rbridge_computedColumnFilterIs,
		rbridge_setDataSetData
	};

	JASPTIMER_START(jaspRCPP_init);

	static std::string tempDirStatic = TempFiles::createTmpFolder();
	static std::string initRCode					= 
			jaspBaseDistributionSamplersR			+ "\n" + 
			jaspBaseFriendlyConstructorFunctionsR	+ "\n" + 
			jaspBaseTransformFunctionsR				+ "\n" + 
			jaspBaseTransformBoxCoxR				+ "\n" + 
			jaspBaseTransformJohnsonR				+ "\n" + 
			jaspBaseTransformYeoJohnsonR			+ "\n" + 
			jaspBaseTransformPowerR;
	// SyntaxInterface uses insideJasp=false for parse/dataset replay and only
	// needs the native callbacks; the Engine still receives the full R helpers.
	const char * initRCodeForMode = insideJasp ? initRCode.c_str() : "";

	Log::log() << "Entering jaspRCPP_init." << std::endl;
	jaspRCPP_init(	AppInfo::getBuildYear()		.c_str(),
					AppInfo::version.asString()	.c_str(),
					&callbacks,
					sendToDesktopFunction,
					pollMessagesFunction,
					[](){ Log::log(false).flush(); return 0;},
					_logWriteFunction,
					rbridge_system,
					rbridge_moduleLibraryFixer,
					resultFont,
					tempDirStatic.c_str(),
					initRCodeForMode,
					insideJasp
	);
	JASPTIMER_STOP(jaspRCPP_init);

}

extern "C" const char * STDCALL rbridge_encodeColumnName(const char * in)
{
	static std::string out;

	if(extraEncodings->shouldEncode(in))	out = extraEncodings->encode(in);
	else									out = ColumnEncoder::columnEncoder()->encode(in);

	return out.c_str();
}

extern "C" const char * STDCALL rbridge_decodeColumnName(const char * in)
{
	//The hand-rolled "extras first, then current encoder" chain is gone: the ONE
	//global strict decode table holds the extras block as a first-class citizen, so a
	//single context-free lookup covers extras, every live filter of every loaded dataset and the
	//fallback encoder. Unknown tokens stay raw - R guards with shouldDecode where it needs
	//to tell the difference.
	static std::string out;
	out = ColumnEncoder::decode(in);

	return out.c_str();
}

extern "C" int STDCALL rbridge_decodeColumnType(const char * in)
{
	static columnType colType;
	colType = ColumnEncoder::columnTypeFromEncoded(in);

	return int(colType);
}

extern "C" bool STDCALL rbridge_shouldEncodeColumnName(const char * in)
{
	return (extraEncodings && extraEncodings->shouldEncode(in)) || ColumnEncoder::columnEncoder()->shouldEncode(in);
}

extern "C" bool STDCALL rbridge_shouldDecodeColumnName(const char * in)
{
	return ColumnEncoder::shouldDecode(in);
}

extern "C" const char * STDCALL rbridge_encodeAllColumnNames(const char * in)
{
	static std::string out;
	out = ColumnEncoder::columnEncoder()->encodeAll(in);
	return out.c_str();
}

extern "C" const char * STDCALL rbridge_decodeAllColumnNames(const char * in)
{
	static std::string out;
	out = ColumnEncoder::decodeAll(in);
	return out.c_str();
}

extern "C" bool STDCALL rbridge_requestJaspResultsFileSource(const char** root, const char **relativePath)
{
	if (!data_bridge)
		return false;

	static std::string _root;
	static std::string _relativePath;

	data_bridge->provideJaspResultsFileName(_root, _relativePath);

	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}


extern "C" bool STDCALL rbridge_requestStateFileSource(const char** root, const char **relativePath)
{
	if (!data_bridge)
		return false;

	static std::string _root;
	static std::string _relativePath;

	data_bridge->provideStateFileName(_root, _relativePath);

	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}

extern "C" bool STDCALL rbridge_requestTempFileName(const char* extensionAsString, const char** root, const char** relativePath)
{
	if (!data_bridge)
		return false;

	static std::string _root, _relativePath;

	data_bridge->provideTempFileName(extensionAsString, _root, _relativePath);
	*root			= _root.c_str();
	*relativePath	= _relativePath.c_str();
	return true;
}

extern "C" bool STDCALL rbridge_requestSpecificFileName(const char* specificFilename, const char** root, const char** relativePath)
{
	if (!data_bridge)
		return false;

	static std::string _root, _relativePath, _specific;

	_specific = specificFilename;

	data_bridge->provideSpecificFileName(_specific, _root, _relativePath);

	*root			= _root.c_str();
	*relativePath	= _relativePath.c_str();
	return true;
}

extern "C" const char* STDCALL rbridge_requestTempRootName()
{
	static std::string _root;
	_root = TempFiles::sessionDirName();

	return _root.c_str();
}

extern "C" bool STDCALL rbridge_runCallback(const char* in, int progress, const char** out)
{
	if (!rbridge_callback)
		return false;

	static std::string staticOut;
	staticOut = rbridge_callback(in, progress);
	*out = staticOut.c_str();

	return true;
}

void rbridge_setWantedCols(const ColumnEncoder::colsPlusTypes& datasetColsTypes)
{
	datasetWanted = datasetColsTypes;
}

std::string rbridge_runModuleCall(const std::string &name, const std::string &title, const std::string &moduleCall, const std::string &dataKey, const std::string &options, const std::string &stateKey, int analysisID, int analysisRevision, bool developerMode, ColumnEncoder::colsPlusTypes datasetColsTypes, bool preloadData, const std::string & multiDataSetJson)
{
	rbridge_callback	= NULL; //Only jaspResults here so callback is not needed
	if (data_bridge != nullptr)
		rbridge_dataSet		= data_bridge->provideAndUpdateDataSet();
	
	datasetWanted = datasetColsTypes;

	return jaspRCPP_runModuleCall(name.c_str(), title.c_str(), moduleCall.c_str(), dataKey.c_str(), options.c_str(), stateKey.c_str(), analysisID, analysisRevision, developerMode, preloadData, multiDataSetJson.c_str());
}

extern "C" RBridgeColumn* STDCALL rbridge_readFullDataSet(size_t * colMax)
{
	return 	rbridge_readFullDataSetHelper(colMax, false);
}

extern "C" RBridgeColumn* STDCALL rbridge_readFullFilteredDataSet(size_t * colMax)
{
	return 	rbridge_readFullDataSetHelper(colMax, true);
}

extern "C" RBridgeColumn* STDCALL rbridge_readFullDataSetHelper(size_t * colMax, bool obeyFilter)
{
	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();

	if(rbridge_dataSet == nullptr)
		return nullptr;

	const Columns & columns = rbridge_dataSet->columns();

	(*colMax) = columns.size();
	RBridgeColumnType* colHeaders = (RBridgeColumnType*)calloc((*colMax), sizeof(RBridgeColumnType));

	for(int i=0; i<(*colMax); i++)
	{
		colHeaders[i].name = strdup(ColumnEncoder::columnEncoder()->encode(columns[i]->name()).c_str());
		colHeaders[i].type = (int)columns[i]->type();
	}

	RBridgeColumn * returnThis = rbridge_readDataSet(colHeaders, (*colMax), obeyFilter);

	for(int i=0; i<(*colMax); i++)
		free(colHeaders[i].name);

	free(colHeaders);

	return returnThis;
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetForComputedColumns(size_t * colMax)
{
	//So yeah, to select a different filter the obeyFilter should be replaced with a filter name, and then that applied.
	//But given that the dropdown only shows using a filter or not and I dont want to make this PR bigger than it already is we take a shortcut here:
	//We just obeyFilter if there is a filter selected. And then later on we can rewrite some more stuff here
	return rbridge_readDataSetForFilteringCompCol(colMax, computedColumnFilter != "");	
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetForFiltering(size_t * colMax)
{
	return rbridge_readDataSetForFilteringCompCol(colMax, false);
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetForFilteringCompCol(size_t * colMax, bool obeyFilter)
{
	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();

	const Columns & columns = rbridge_dataSet->columns();

	datasetWanted.clear();

	for(const std::string & colName : filterColumnsUsed)
		if(rbridge_dataSet->column(colName))
			datasetWanted.insert(std::make_pair(colName, rbridge_dataSet->column(colName)->type()));
		else if(!colName.empty())
		{
			//It should then be a columnName + '.' + type
			//And decoding always goes to the original columnname so:
			std::string colNameSansType = ColumnEncoder::decode(ColumnEncoder::columnEncoder()->encode(colName));

			assert(colName[colNameSansType.size()] == '.'); //Because how??? ;)

			std::string typeStr = colName.substr(colNameSansType.size() + 1);

			assert(columnTypeValidName(typeStr));

			datasetWanted.insert(std::make_pair(colName, columnTypeFromString(typeStr)));
		}

	return rbridge_readDataSetRequested(colMax, obeyFilter);
}

static RBridgeColumn*	datasetStatic = nullptr;
static int				datasetColMax = 0;

extern "C" RBridgeColumn* STDCALL rbridge_readDataSet(RBridgeColumnType* colHeaders, size_t colMax, bool obeyFilter)
{
	if (colHeaders == nullptr)
		return nullptr;

	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();

	if(rbridge_dataSet == nullptr)
		return nullptr;

	const Columns & columns = rbridge_dataSet->columns();

	if (datasetStatic != nullptr)
		freeRBridgeColumns();

	datasetColMax = colMax;
	datasetStatic = static_cast<RBridgeColumn*>(calloc(datasetColMax + 1, sizeof(RBridgeColumn)));

	size_t filteredRowCount = obeyFilter ? rbridge_dataSet->shownFilter()->filteredRowCount() : rbridge_dataSet->rowCount();

	// lets make some rownumbers/names for R that takes into account being filtered or not!
	datasetStatic[colMax].ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));
	datasetStatic[colMax].nbRows	= filteredRowCount;
	int filteredRow					= 0;

	//If you change anything here, make sure that "label outliers" in Descriptives still works properly (including with filters)
	for(size_t i=0; i<rbridge_dataSet->rowCount() && filteredRow < datasetStatic[colMax].nbRows; i++)
		if(
			!obeyFilter ||
					(rbridge_dataSet->shownFilter()->filtered().size() > i && rbridge_dataSet->shownFilter()->filtered()[i])
		)
			datasetStatic[colMax].ints[filteredRow++] = int(i + 1); //R needs 1-based index

	//std::cout << "reading " << colMax << " columns!\nRowCount: " << filteredRowCount << "" << std::endl;

	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType	&	columnInfo		= colHeaders[colNo];
		RBridgeColumn		&	resultCol		= datasetStatic[colNo];
		std::string				columnName		= ColumnEncoder::decode(columnInfo.name);	//context-free table
								resultCol.name	= strdup(columnInfo.name);
		Column				*	column			= rbridge_dataSet->column(columnName);

		if(!column)
			throw std::runtime_error("rbridge_readDataSet gets '" + std::string(columnInfo.name) + "' but it does not name a column of the current dataset");

		columnType				colType			= column->type(),
								requestedType	= columnType(columnInfo.type);

		if (requestedType == columnType::unknown)
			requestedType = colType;

		resultCol.nbRows		= filteredRowCount;
		resultCol.dropLevels	= column->shouldDropLevels();
		
		if (requestedType == columnType::scale)
		{
			int rowNo = 0;
					
			resultCol.isScale	= true;
			resultCol.doubles	= (double*)calloc(filteredRowCount, sizeof(double));
			
			boolvec filterToUse;
			if(obeyFilter)
				filterToUse = rbridge_dataSet->shownFilter()->filtered();

			for(double value : column->dataAsRDoubles(filterToUse))
				resultCol.doubles[rowNo++] = value;
		}
		else // if (requestedType != ColumnType::scale)
		{
			resultCol.isScale	= false;
			resultCol.ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));
			resultCol.isOrdinal = (requestedType == columnType::ordinal);
			
			intvec vals;
			boolvec filterToUse;
			if(obeyFilter)
				filterToUse = rbridge_dataSet->shownFilter()->filtered();
			
			stringvec levels = column->dataAsRLevels(vals, filterToUse);
			
			memcpy(resultCol.ints, vals.data(), vals.size() * sizeof(int));
			
			resultCol.labels = rbridge_getLabels(levels, resultCol.nbLabels);
		}
	}

	return datasetStatic;
}

//This is straight up copied from jasprcpp, but thanks to the split between rbridge and jasprcpp this is how its gonna be...
void __freeRBridgeColumnType(RBridgeColumnType *columns, size_t colMax)
{
	for (int i = 0; i < colMax; i++)
		free(columns[i].name);

	delete[](columns);
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetRequested(size_t * colMax, bool obeyFilter)
{
	//Point the process-global ColumnEncoder at the shown dataset before encoding the wanted columns
	//below. rbridge_readDataSet() at the end of this function does this too, but by then the names
	//have already been encoded - against the empty fallback encoder if nothing else provided the
	//dataset first. See the invariant documented in DataBridge::provideAndUpdateDataSet().
	//
	//In a multi-dataset aware run the queued slices are handed out one read at a time: jaspBase's
	//runJaspResults calls this once per referenced dataset and we switch the engine over to that
	//dataset, its selected filter and its wanted columns before reading.
	const DataBridge::MultiDataSetSlice * slice = data_bridge ? data_bridge->takeMultiDataSetSlice() : nullptr;

	// on a queued slice read, the wanted columns are encoded through the SLICE
	//FILTER's explicit view handle - not through whatever the process-global current encoder
	//happens to point at. The re-anchoring of currentEncoder below stays for the legacy encode
	//callbacks R fires during the read (the fallback indirection), but the slice encode here no
	//longer depends on that ordering.
	ColumnEncoder * sliceEncoder = nullptr;

	if(slice)
	{
		rbridge_dataSet = data_bridge->provideAndUpdateDataSet(slice->dataSetId);

		if(rbridge_dataSet)
		{
			Filter * filter = slice->filterId >= 0 && data_bridge->workspace() ? data_bridge->workspace()->filterById(slice->filterId) : nullptr;

			//Apply the filter recorded for this slice explicitly (and fall back to the default one):
			//the dataset may still have some other filter shown from a previous run. Only show
			//filters that exist; showFilter(name) would fabricate unknown ones (see Engine::runAnalysis).
			if(filter && filter->data() == rbridge_dataSet && rbridge_dataSet->filter(filter->name()))
			{
				rbridge_dataSet->showFilter(filter->name());
				sliceEncoder = filter->columnEncoder();			//the explicit handle of the slice's own filter
			}
			else if(rbridge_dataSet->defaultFilter())
			{
				rbridge_dataSet->showFilter(rbridge_dataSet->defaultFilter());
				sliceEncoder = rbridge_dataSet->defaultFilter()->columnEncoder();
			}

			//showFilter() ran after provideAndUpdateDataSet() re-pointed the indirection, so anchor
			//it at the slice's own view now too - same handle sliceEncoder resolves to above, kept
			//for the R-side legacy encode entries.
			if(ColumnEncoder * view = rbridge_dataSet->shownEncoder())
				ColumnEncoder::setCurrentEncoder(view);

			datasetWanted = slice->cols;
		}
	}
	else if(data_bridge)
		rbridge_dataSet = data_bridge->provideAndUpdateDataSet();

	*colMax = 0;
	RBridgeColumnType * requestedColumns = new RBridgeColumnType[datasetWanted.size()];

    for(const auto & nameType : datasetWanted)
    {
		ColumnEncoder * encoder = sliceEncoder ? sliceEncoder : ColumnEncoder::columnEncoder();

		//Make sure this is encodable/decodable
		if(!encoder->shouldEncode(nameType.first) && !ColumnEncoder::shouldDecode(nameType.first))
			throw std::runtime_error("rbridge_readDataSetRequested gets '" + nameType.first + "' but its not a columnname at all");
		
		requestedColumns[*colMax].name = strdup(encoder->encodeAll(nameType.first).c_str());
        requestedColumns[*colMax].type = int(nameType.second);

        (*colMax)++;
    }

	RBridgeColumn * theData = rbridge_readDataSet(requestedColumns, *colMax, obeyFilter);

	__freeRBridgeColumnType(requestedColumns, *colMax);

	return theData;
}

extern "C" char** STDCALL rbridge_readDataColumnNames(size_t * colMax)
{
	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();

	if(!rbridge_dataSet)
	{
		*colMax = 0;
		return nullptr;
	}

	const Columns &	columns			= rbridge_dataSet->columns();
	static int		staticColMax	= 0;
	static char	**	staticResult	= nullptr;

	if (staticResult)
	{
		for (int i = 0; i < staticColMax; i++)
			free(staticResult[i]);
		free(staticResult);
	}
	staticColMax = rbridge_dataSet->columnCount();
	staticResult = (char**)calloc(staticColMax, sizeof(char*));

	int colNo = 0;
	for (const Column * column: columns)
		staticResult[colNo++] = strdup(ColumnEncoder::columnEncoder()->encode(column->name()).c_str());

	*colMax = staticColMax;
	return staticResult;
}

extern "C" RBridgeColumnDescription* STDCALL rbridge_readDataSetDescription(RBridgeColumnType* columnsType, size_t colMax)
{
	if (!columnsType)
		return nullptr;

	static size_t						lastColMax = 0;
	static RBridgeColumnDescription	*	resultCols = nullptr;

	if (resultCols != nullptr)
		freeRBridgeColumnDescription(resultCols, lastColMax);

	lastColMax				= colMax;
	resultCols				= static_cast<RBridgeColumnDescription*>(calloc(colMax, sizeof(RBridgeColumnDescription)));
	rbridge_dataSet			= data_bridge->provideAndUpdateDataSet();
	const Columns & columns	= rbridge_dataSet->columns();

	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType			&	columnInfo		= columnsType[colNo];
		RBridgeColumnDescription	&	resultCol		= resultCols[colNo];
		//Context-free table decode. The old instance-rooted decode threw on a
		//miss; the table keeps unknown tokens raw, so translate that failure mode into the same
		//explicit error instead of dereferencing a null column below.
		std::string					columnName		= ColumnEncoder::decode(columnInfo.name);
										resultCol.name	= strdup(columnInfo.name);
		Column						*	column			= rbridge_dataSet->column(columnName);

		if(!column)
			throw std::runtime_error("rbridge_readDataSetDescription gets '" + std::string(columnInfo.name) + "' but it does not name a column of the current dataset");

		columnType						colType			= column->type(),
										requestedType	= columnType(columnInfo.type);

		if (requestedType == columnType::unknown)
			requestedType = colType;


		resultCol.isScale	= requestedType == columnType::scale;
		resultCol.isOrdinal = requestedType == columnType::ordinal; 
		resultCol.labels 	= rbridge_getLabels(column->labels(), resultCol.nbLabels);
	}

	return resultCols;
}

///Sneaky variable declaration of colName! Decoding is the context-free global table.
#define JASP_COLUMN_DECODE_HERE_STORED_colName std::string colName(ColumnEncoder::decode(columnName))

extern "C" int STDCALL rbridge_getColumnType(const char * columnName)
{
	if(!ColumnEncoder::shouldDecode(columnName))
		return data_bridge->getColumnType(columnName);

	JASP_COLUMN_DECODE_HERE_STORED_colName;
	return data_bridge->getColumnType(colName);
}

extern "C" int STDCALL rbridge_getColumnAnalysisId(const char * columnName)
{
	if(!ColumnEncoder::shouldDecode(columnName))
		return  data_bridge->getColumnAnalysisId(columnName);

	JASP_COLUMN_DECODE_HERE_STORED_colName;
	return data_bridge->getColumnAnalysisId(colName);
}


extern "C" int STDCALL rbridge_getColumnOriginalIndex(const char * columnName)
{
	if(!ColumnEncoder::shouldDecode(columnName))
		return  data_bridge->getColumnOriginalIndex(columnName);

	JASP_COLUMN_DECODE_HERE_STORED_colName;
	return data_bridge->getColumnOriginalIndex(colName);
}

extern "C" const char * STDCALL rbridge_createColumn(const char * columnName, bool computed)
{
	static std::string lastColumnName;
	lastColumnName = data_bridge->createColumn(columnName, computed);

	return lastColumnName.c_str();
}

extern "C" bool STDCALL rbridge_deleteColumn(const char * columnName)
{
	return data_bridge->deleteColumn(columnName);
}

extern "C" bool STDCALL rbridge_setColumnDataAndType(const char* columnName, const char ** nominalData, size_t length, int _columnType, bool computed)
{
	JASP_COLUMN_DECODE_HERE_STORED_colName;

	std::vector<std::string> nominals(nominalData, nominalData + length);

	return data_bridge->setColumnDataAndType(colName, nominals, columnType(_columnType), computed);
}

extern "C" bool STDCALL rbridge_setDataSetData(const char* datasetName, const char ** columnNames, const int * columnTypes, const char *** columnData, const size_t * columnLengths, size_t colCount)
{
	std::vector<std::string>							names;
	std::vector<columnType>								types;
	std::vector<std::vector<std::string>>				data;

	types.reserve(colCount);
	data.reserve(colCount);
	names.reserve(colCount);

	for(size_t i=0; i<colCount; i++)
	{
		//The R data.frame's column names are encoded (the user code was encoded before evaluation);
		//decode them back to the plain names (no type suffix) so the output dataset gets the real
		//column titles. New/user-authored names are not in the decode table, so keep them as-is.
		const std::string & colName = columnNames[i];
		names.push_back(ColumnEncoder::shouldDecode(colName)
			? ColumnEncoder::decode(colName)
			: colName);

		types.push_back(columnType(columnTypes[i]));
		data.emplace_back(columnData[i], columnData[i] + columnLengths[i]);
	}

	return data_bridge->setDataSet(datasetName, names, types, data);
}

extern "C" int	STDCALL rbridge_dataSetRowCount()
{
	return data_bridge->dataSetRowCount();
}

void rbridge_memoryCleaning()
{
	freeRBridgeColumns();
	jaspRCPP_purgeGlobalEnvironment();
}

void rbridge_clearDataSet()
{
	rbridge_dataSet = nullptr;
}

void freeRBridgeColumns()
{
	if(datasetStatic == nullptr)
		return;

	for (int i = 0; i < datasetColMax; i++)
	{
		RBridgeColumn& column = datasetStatic[i];
		free(column.name);
		if (column.isScale)	free(column.doubles);
		else				free(column.ints);

		if (!column.isScale)
			freeLabels(column.labels, column.nbLabels);
	}
	free(datasetStatic[datasetColMax].ints); //rownames/numbers
	free(datasetStatic);

	datasetStatic	= nullptr;
	datasetColMax	= 0;
}

void freeRBridgeColumnDescription(RBridgeColumnDescription* columns, size_t colMax)
{
	for (int i = 0; i < colMax; i++)
	{
		RBridgeColumnDescription& column = columns[i];
		free(column.name);
		if (!column.isScale)
			freeLabels(column.labels, column.nbLabels);
	}
	free(columns);
}

void freeLabels(char** labels, size_t nbLabels)
{
	for (int i = 0; i < nbLabels; i++)
		free(labels[i]);
	free(labels);
}

char** rbridge_getLabels(const Labels & levels, size_t &nbLevels)
{
	char** results = nullptr;
	nbLevels = 0;
	if (levels.size() == 0)
	{
		results = (char**)calloc(1, sizeof(char*));
		results[0] = strdup(".");
	}
	else
	{
		size_t levelsNotEmpty = levels.size();
		for(const Label * label : levels)
			if(label->isEmptyValue())
				levelsNotEmpty--;
		
		results = (char**)calloc(levelsNotEmpty, sizeof(char*));
		int i = 0;
		for (const Label * level: levels)
		if(!level->isEmptyValue())
			results[i++] = strdup(level->label().c_str());
		nbLevels = i;
	}

	return results;
}

char** rbridge_getLabels(const std::vector<std::string> &levels, size_t &nbLevels)
{
	char** results = nullptr;
	nbLevels = 0;
	if (levels.size() == 0)
	{
		results = (char**)calloc(1, sizeof(char*));
		results[0] = strdup(".");
	}
	else
	{
		results = (char**)calloc(levels.size(), sizeof(char*));
		int i = 0;
		for (const std::string &level: levels)
			results[i++] = strdup(level.c_str());
		nbLevels = i;
	}

	return results;
}

std::string	rbridge_encodeColumnNamesInScript(const std::string & filterCode, ColumnEncoder * sliceEncoder)
{
	// slice-aware callers pass the run's OWN filter view (the host-bound fallback
	//namespace of the run - what its bare names mean); nullptr keeps the legacy fallback,
	//the process-global current encoder, for callers that are not aware (byte-identical to
	//the legacy behaviour). The found columns land in filterColumnsUsed either way - that set is what
	//the own data read wants.
	return (sliceEncoder ? sliceEncoder : ColumnEncoder::columnEncoder())->encodeRScript(filterCode, &filterColumnsUsed);
}

void rbridge_setupRCodeEnv(int rowCount, const std::string & dataname)
{
	static std::string setupFilterEnv;

	setupFilterEnv =	"rowcount    <- " + std::to_string(rowCount) +  ";";
	jaspRCPP_runScript(setupFilterEnv.c_str());

	rbridge_setupRCodeEnvReadData(dataname, ".readCompColDatasetToEnd()");
}

void rbridge_setupRCodeFilterEnv(int rowCount, const std::string & dataname)
{
	static std::string setupFilterEnv;

	setupFilterEnv =	"rowcount    <- " + std::to_string(rowCount) +  ";";
	jaspRCPP_runScript(setupFilterEnv.c_str());

	rbridge_setupRCodeEnvReadData(dataname, ".readFilterDatasetToEnd()");
}


void rbridge_setupRCodeEnvReadData(const std::string & dataname, const std::string & readFunction)
{
	static std::string setupFilterEnv;

	setupFilterEnv =	dataname + " <- " + readFunction + ";\n"
						"attach(" + dataname + ");"																"\n"
						"options(warn=1, showWarnCalls=TRUE, showErrorCalls=TRUE, show.error.messages=TRUE);"	"\n";

	jaspRCPP_runScript(setupFilterEnv.c_str());
}

void rbridge_detachRCodeEnv(const std::string & dataname)
{
	static std::string detacher;
	detacher = "detach("+dataname+")";
	jaspRCPP_runScript(detacher.c_str());	//and afterwards we make sure it is detached to avoid superfluous messages and possible clobbering of analyses
}

bool rbridge_prepareSliceRunScript(const std::string & code, Filter * ownFilter, Workspace * workspace, const std::vector<int> & referencedFilterIds, std::string & encodedScript, std::vector<DataBridge::MultiDataSetSlice> & slices, std::string & error)
{
	// The engine-side consumption of a filter/compute run's
	//code. The desktop scanned this very text at request build (Workspace::
	//referencedFilterIdsInCode) and stamped the transient `referencedFilterIds` sidecar onto
	//the request; this function re-scans the code (one shared scanner, so the two sides cannot
	//disagree about what a reference IS), resolves every reference against the engine's own
	//workspace, rewrites each surface form into the referenced filter view's token and queues
	//the slices. The strip discipline (mirroring the last-moment placement): the
	//sidecar ids live only in this function's inputs and the queue it produces - the returned
	//`encodedScript` is what R evaluates, and it carries tokens (the sanctioned
	//provenance-in-name namespace), never the sidecar, never surface forms.
	encodedScript.clear();
	slices.clear();
	error.clear();

	if(!ownFilter || !ownFilter->data() || !workspace || !ownFilter->columnEncoder())
	{
		error = "The run has no slice context to encode its code against (own filter, dataset or encoder missing).";
		return false;
	}

	DataSet * ownDataSet	= ownFilter->data();
	const int ownFilterId	= ownFilter->id();

	//Load every sidecar-referenced dataset once BEFORE resolving: the engine workspace only
	//skeletonises datasets it has never provided (Workspace::checkForUpdates creates the
	//DataSet but the first per-dataset checkForUpdates does the actual dbLoad), so a foreign
	//dataset that no request has touched yet would make every resolution below fail with a
	//bogus unknownDataSet. Same "load every involved dataset" pass prepareMultiDataSetRun
	//does before it encodes against the slices.
	for(const int filterId : referencedFilterIds)
		if(filterId != ownFilterId)
		{
			const int refDataSetId = workspace->db().filterGetDataSetId(filterId);

			if(refDataSetId >= 0)
				if(DataSet * refDataSet = workspace->dataSetById(refDataSetId))
					refDataSet->checkForUpdates();
		}

	std::string worked = code;

	//Per referenced FOREIGN filter: the columns the code references there (name -> the
	//resolved type, type segment wins). Reverse iteration: the replacements below shift byte
	//positions, and the occurrences are disjoint and ordered - replacing back to front keeps
	//every earlier position valid.
	std::map<int, ColumnEncoder::colsPlusTypes> colsPerForeignFilter;

	//Qualified SELF-references (resolved to the run's own filter) ride the own slice: their
	//columns must be read into the own data frame even though the bare-name pass below can
	//never see them (the surface form is rewritten into a token BEFORE that pass runs, and a
	//token is not a name the encode pass would recognize).
	ColumnEncoder::colsPlusTypes ownQualifiedCols;

	const std::vector<QualifiedRefOccurrence> occurrences = QualifiedColumnRef::scanText(worked);

	for(auto occurrence = occurrences.rbegin(); occurrence != occurrences.rend(); ++occurrence)
	{
		ResolvedQualifiedColumn resolved;
		QString resolveError;
		const QualifiedRefError kind = workspace->resolveQualifiedColumnRef(occurrence->ref, resolved, resolveError);

		if(kind != QualifiedRefError::none)
		{
			//LOUD: a reference that cannot resolve at run time fails
			//the run with the typed ready-to-show message - no crash, and no silent
			//fallback to raw names (those would die in R as an untyped "object not found").
			error = "The reference " + fq(occurrence->ref.render()) + " in the code does not resolve: " + fq(resolveError);
			return false;
		}

		const bool selfReference = resolved.filterId == ownFilterId;

		if(!selfReference && std::find(referencedFilterIds.begin(), referencedFilterIds.end(), resolved.filterId) == referencedFilterIds.end())
		{
			// the desktop owns provenance - it scanned this code at request build
			//and stamped the sidecar. A reference outside it means the request is stale (the
			//code or the workspace changed since the request was queued). Refusing to guess
			//beats silently evaluating against a slice that was never materialized.
			error = "The reference " + fq(occurrence->ref.render()) + " resolves to filter " + std::to_string(resolved.filterId)
					+ ", which this run's request did not list as a referenced slice (stale request?). Trigger the run again to refresh it.";
			return false;
		}

		if(selfReference)
			ownQualifiedCols.insert({ resolved.columnName, resolved.type });

		//Resolve the live filter per use (the no-caching landmine).
		Filter * refFilter = workspace->filterById(resolved.filterId);	//materialises named filters of reopened files

		if(!refFilter || !refFilter->data() || !refFilter->columnEncoder())
		{
			error = "The reference " + fq(occurrence->ref.render()) + " resolves to filter " + std::to_string(resolved.filterId) + ", which is no longer available in the engine's workspace.";
			return false;
		}

		//The surface form becomes the referenced filter view's unique token (globally
		//unique by construction). In R it resolves through the attached slice frame - or
		//through the own data frame for a self-reference (same namespace: the own context IS
		//the host-bound fallback, so a qualified self-reference is just an explicit bare name).
		const std::string token = refFilter->columnEncoder()->encode(resolved.columnName);
		worked.replace(occurrence->begin, occurrence->end - occurrence->begin, token);

		if(!selfReference)
			colsPerForeignFilter[resolved.filterId].insert({ resolved.columnName, resolved.type });
	}

	//The bare-name pass over the substituted code - through the run's OWN filter view (the host-bound fallback),
	//never the process-global current encoder - fills filterColumnsUsed for the own read.
	encodedScript = rbridge_encodeColumnNamesInScript(worked, ownFilter->columnEncoder());

	//The queue: one read-slice per referenced FOREIGN filter, in the sidecar's order (the
	//desktop scan deduplicated), each with the columns the code references in it.
	for(const int filterId : referencedFilterIds)
	{
		if(filterId == ownFilterId)
			continue;	//self-references ride the own slice; no foreign frame for them

		const auto cols = colsPerForeignFilter.find(filterId);

		if(cols == colsPerForeignFilter.end())
			continue;	//the sidecar names it but the code does not reference it (cannot happen on the same text; belt)

		Filter * sliceFilter = workspace->filterById(filterId);

		if(!sliceFilter || !sliceFilter->data())
		{
			//A dead slice in the sidecar: the code cannot reference it (that reference failed
			//resolution above), so skipping is safe - same doctrine as prepareMultiDataSetRun.
			Log::log() << "rbridge_prepareSliceRunScript: the referenced filter " << filterId << " does not resolve to a dataset anymore; its slice is skipped." << std::endl;
			continue;
		}

		slices.push_back({ sliceFilter->data()->id(), filterId, cols->second });
	}

	//The run's OWN slice, LAST: the setup read pops it, so the own data frame is encoded in
	//ownFilter's view (the output name and the data frame must share the
	//same namespace). Its wanted columns are exactly what the bare pass found - the same set
	//the legacy rbridge_readDataSetForFilteringCompCol built from filterColumnsUsed, typed
	//from the own dataset.
	ColumnEncoder::colsPlusTypes ownCols;

	for(const std::string & colName : filterColumnsUsed)
	{
		if(const Column * column = ownDataSet->column(colName))
			ownCols.insert({ colName, column->type() });
		else if(!colName.empty() && colName.find('.') != std::string::npos)
		{
			//A "name.type" entry (the legacy wanted-cols contract): keep it with its spelled type.
			const std::string colNameSansType	= ColumnEncoder::decode(ownFilter->columnEncoder()->encode(colName));
			const std::string typeSpelling		= colName.size() > colNameSansType.size() + 1 ? colName.substr(colNameSansType.size() + 1) : "";

			if(columnTypeValidName(typeSpelling))
				ownCols.insert({ colName, columnTypeFromString(typeSpelling) });
		}
	}

	//Plus whatever the qualified SELF-references demand (see ownQualifiedCols above).
	ownCols.insert(ownQualifiedCols.begin(), ownQualifiedCols.end());

	slices.push_back({ ownDataSet->id(), ownFilterId, ownCols });

	return true;
}

void rbridge_attachReferencedSlices(const std::vector<DataBridge::MultiDataSetSlice> & slices)
{
	// The new producer of slice queues for filter/compute
	//runs. `slices` (foreign slices first, the run's own last) becomes THE queue that
	//rbridge_readDataSetRequested's slice handling (unchanged) consumes one read
	//per slice; every FOREIGN slice is attached as its own frame under its slice key so the
	//foreign tokens resolve on R's search path. The OWN slice stays queued: the setup read
	//(`.readFilterDatasetToEnd()`/`.readCompColDatasetToEnd()`) pops it, which also re-points
	//the shown dataset back home after the foreign reads and anchors the legacy R-side encode
	//indirection at the own view.
	if(!data_bridge || slices.empty())
		return;

	data_bridge->setMultiDataSetQueue(slices);

	for(size_t i = 0; i + 1 < slices.size(); ++i)
	{
		const std::string frame = "slice_" + std::to_string(slices[i].filterId);

		//A previous aborted run may have left the frame attached; detach it best-effort first
		//(attach() would otherwise stack a masked version 2 of the same name). NB: the read
		//entry is called directly (it is registered in the runScript-visible rEnvironment like
		//.readCompColDatasetToEnd et al.) - .fromRCPP is a jaspBase-namespace function and NOT
		//reachable from these raw scripts.
		jaspRCPP_runScript(("try(detach('" + frame + "'), silent=TRUE); "
							+ frame + " <- .readDataSetRequestedNative(); "
							+ "attach(" + frame + ");").c_str());
	}
}

void rbridge_detachReferencedSlices(const std::vector<DataBridge::MultiDataSetSlice> & slices)
{
	if(slices.empty())
		return;

	for(size_t i = 0; i + 1 < slices.size(); ++i)
		jaspRCPP_runScript(("try(detach('slice_" + std::to_string(slices[i].filterId) + "'), silent=TRUE);").c_str());
}

std::vector<bool> rbridge_applyFilter(const std::string & filterCode, const std::string & generatedFilterCode, Filter * ownFilter, const std::vector<int> & referencedFilterIds)
{
	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();

	if(rbridge_dataSet == nullptr)
		throw filterException("rbridge_dataSet == nullptr!");

	int rowCount = rbridge_dataSet->rowCount();

	if(filterCode == "*" || filterCode == "") //if * then there is no filter so everything is fine :)
		return std::vector<bool>(rowCount, true);

	static std::string errorMsg;

	// the run's slice context is the filter BEING RUN - fixes the filter
	//expression's bare-name fallback to its own dataset's filter - so the code encodes
	//through THAT filter's view, not the process-global current encoder. `ownFilter` is it;
	//the legacy fallback (nullptr) is the shown filter, which is the same filter on every
	//caller that shows it first (runFilterByName does).
	Filter * contextFilter = ownFilter ? ownFilter : rbridge_dataSet->shownFilter();

	std::string								filter64,
											prepError;
	std::vector<DataBridge::MultiDataSetSlice>	slices;

	if(!rbridge_prepareSliceRunScript(generatedFilterCode + "\n" + filterCode, contextFilter, data_bridge->workspace(), referencedFilterIds, filter64, slices, prepError))
		throw filterException(prepError.c_str());	//loud: a reference that does not resolve fails the run

	filter64 = "local({" + filter64 + "})";

	R_FunctionWhiteList::scriptIsSafe(filter64); //can throw filterExceptions

	bool * arrayPointer = nullptr;

	rbridge_attachReferencedSlices(slices);	//foreign frames attached; the own slice queued for the setup read
	rbridge_setupRCodeFilterEnv(rowCount);
	int arrayLength	= jaspRCPP_runFilter(filter64.c_str(), &arrayPointer);
	rbridge_detachReferencedSlices(slices);
	rbridge_detachRCodeEnv();

	if(arrayLength < 0)
	{
		errorMsg = ColumnEncoder::decodeAll(jaspRCPP_getLastErrorMsg());

		if(errorMsg == "")
			errorMsg = "Filter returned something incomprehensible, make sure you entered all columnnames *exactly* right.";

		throw filterException(errorMsg.c_str());
	}
	
	std::vector<bool> returnThis;

	

	if(arrayLength != rowCount)
	{
		std::stringstream msg;
		msg << "Filter did not return a logical vector of length " << rowCount << " as expected, instead it returned a logical vector of length " << arrayLength << std::endl;
		errorMsg = msg.str();
		throw filterException(errorMsg);
	}
	
	for(size_t i=0; i<arrayLength; i++)
		returnThis.push_back(arrayPointer[i]);
	
	jaspRCPP_freeArrayPointer(&arrayPointer);

	return returnThis;
}

std::string rbridge_evalRCodeWhiteListed(const std::string & rCode, bool setWd, Filter * ownFilter, const std::vector<int> & referencedFilterIds)
{
	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();
	int rowCount	= rbridge_dataSet == nullptr ? 0 : rbridge_dataSet->rowCount();

	jaspRCPP_resetErrorMsg();

	// (design decision 6): the encode runs against an EXPLICIT slice context -
	//the caller's filter, else the shown dataset's shown filter (exactly what
	//provideAndUpdateDataSet anchored the legacy indirection at) - closing the process-global
	//side door: this path no longer READS the current encoder, whatever it points at.
	Filter * contextFilter = ownFilter ? ownFilter : (rbridge_dataSet ? rbridge_dataSet->shownFilter() : nullptr);

	std::string rCode64;

	std::vector<DataBridge::MultiDataSetSlice>	slices;

	if(contextFilter)
	{
		std::string prepared,
					prepError;

		if(!rbridge_prepareSliceRunScript(rCode, contextFilter, data_bridge->workspace(), referencedFilterIds, prepared, slices, prepError))
		{
			//Loud: an unresolvable reference fails the eval with the
			//typed message - never a silent fallback.
			jaspRCPP_setErrorMsg(prepError.c_str());
			return "The R code could not be run: " + prepError;
		}

		rCode64 = "local({" + prepared + "})";
	}
	else
		rCode64 = "local({" + rbridge_encodeColumnNamesInScript(rCode) + "})";	//no dataset at all: nothing to slice, the legacy encode (empty encoder, as before)

	try							{ R_FunctionWhiteList::scriptIsSafe(rCode64); }
	catch(filterException & e)	{ jaspRCPP_setErrorMsg(e.what()); return std::string("R code is not safe because of: ") + e.what();	}


	rbridge_attachReferencedSlices(slices);
	rbridge_setupRCodeEnv(rowCount);
	std::string result = jaspRCPP_evalRCode(rCode64.c_str(), setWd);
	rbridge_detachReferencedSlices(slices);
	jaspRCPP_runScript("detach(data)");	//and afterwards we make sure it is detached to avoid superfluous messages and possible clobbering of analyses

	jaspRCPP_setErrorMsg(ColumnEncoder::decodeAll(jaspRCPP_getLastErrorMsg()).c_str());

	return result;
}


std::string rbridge_evalRComputedColumn(const std::string &rCode, const std::string & setColumnFunc, const std::string & filterToUse, Filter * ownFilter, const std::vector<int> & referencedFilterIds)
{
	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();
	
	if(!rbridge_dataSet)
		return "null"; // How would doing a computed column make any sense without data?
	
	computedColumnFilter = filterToUse == "" ? DEFAULT_FILTER_NAME : filterToUse;
	
	rbridge_dataSet->showFilter(computedColumnFilter);

	//The row count of the run's OWN slice - captured before any referenced slice is
	//materialized (those reads re-point the shown dataset; the returned boolvec must be this
	//long, rbridge.cpp's own length check is the O3 loud guardrail for row-mismatched
	//element-wise foreign references).
	int rowCount	= rbridge_dataSet->shownFilter()->filteredRowCount();

	jaspRCPP_resetErrorMsg();

	// The run's slice context is the COMPUTE FILTER - the
	//caller resolved it (`ownFilter`); the fallback is the filter just shown (the same thing).
	//The data frame R reads and the output-name encode (Engine::runComputeColumn) use this
	//same view; the process-global current encoder is no longer consulted on this path.
	Filter * contextFilter = ownFilter ? ownFilter : rbridge_dataSet->shownFilter();

	std::string								prepared,
											prepError;
	std::vector<DataBridge::MultiDataSetSlice>	slices;

	if(!rbridge_prepareSliceRunScript(rCode, contextFilter, data_bridge->workspace(), referencedFilterIds, prepared, slices, prepError))
	{
		//Loud: fail the run with the typed message, no silent fallback.
		computedColumnFilter = "";
		throw filterException(prepError.c_str());
	}

	std::string rCode64(prepared);
	
	try							{ R_FunctionWhiteList::scriptIsSafe(rCode64); }
	catch(filterException & e)	{ jaspRCPP_setErrorMsg(e.what()); computedColumnFilter = ""; return std::string("R code is not safe because of: ") + e.what();	}


	rbridge_attachReferencedSlices(slices);
	rbridge_setupRCodeEnv(rowCount);
	std::string result = jaspRCPP_evalComputedColumn(rCode64.c_str(), setColumnFunc.c_str());
	rbridge_detachReferencedSlices(slices);
	jaspRCPP_runScript("detach(data)");	//a	nd afterwards we make sure it is detached to avoid superfluous messages and possible clobbering of analyses

	jaspRCPP_setErrorMsg(ColumnEncoder::decodeAll(jaspRCPP_getLastErrorMsg()).c_str());

	computedColumnFilter = "";
	
	return result;
}

std::string rbridge_evalRComputedDataSet(const std::string & rCode, const std::string & outputDataSetName, const std::string & filterToUse, Filter * ownFilter, const std::vector<int> & referencedFilterIds)
{
	rbridge_dataSet = data_bridge->provideAndUpdateDataSet();

	if(!rbridge_dataSet)
		return "null";

	computedColumnFilter = filterToUse.empty() ? DEFAULT_FILTER_NAME : filterToUse;

	rbridge_dataSet->showFilter(computedColumnFilter);

	int rowCount	= rbridge_dataSet->shownFilter()->filteredRowCount();

	jaspRCPP_resetErrorMsg();

	// The run's slice context is the INPUT filter (the
	//existing defaultInputFilterId dropdown is that anchor); the caller resolved it. Same
	//explicit-handle discipline as the computed-column path above.
	Filter * contextFilter = ownFilter ? ownFilter : rbridge_dataSet->shownFilter();

	std::string								prepared,
											prepError;
	std::vector<DataBridge::MultiDataSetSlice>	slices;

	if(!rbridge_prepareSliceRunScript(rCode, contextFilter, data_bridge->workspace(), referencedFilterIds, prepared, slices, prepError))
	{
		//Loud: fail the run with the typed message, no silent fallback.
		computedColumnFilter = "";
		throw filterException(prepError.c_str());
	}

	std::string rCode64(prepared);

	try							{ R_FunctionWhiteList::scriptIsSafe(rCode64); }
	catch(filterException & e)	{ jaspRCPP_setErrorMsg(e.what()); computedColumnFilter = ""; return std::string("R code is not safe because of: ") + e.what();	}

	rbridge_attachReferencedSlices(slices);

	rbridge_setupRCodeEnv(rowCount);

	//The user code is expected to produce a data.frame (kept in .jaspResult), which is then written
	//into the output (computed) dataset by name. The name is user-editable, so escape it for use inside
	//a single-quoted R string to avoid breaking the generated code on ' \ or newlines.
	std::string escapedName = outputDataSetName;
	{
		std::string out;
		out.reserve(escapedName.size());
		for (char c : escapedName)
		{
			switch (c)
			{
			case '\\':	out += "\\\\";	break;
			case '\'':	out += "\\'";	break;
			case '\n':	out += "\\n";	break;
			case '\r':	out += "\\r";	break;
			case '\t':	out += "\\t";	break;
			default:	out += c;		break;
			}
		}
		escapedName = out;
	}
	//Wrap in toString() because .setDataSet returns an R logical, and
	//jaspRCPP_parseEvalStringReturn only returns a string result (else "null").
	std::string setDataSetCode = "toString(.setDataSet('" + escapedName + "', .jaspResult))";
	std::string result = jaspRCPP_evalComputedDataSet(rCode64.c_str(), setDataSetCode.c_str());
	rbridge_detachReferencedSlices(slices);
	jaspRCPP_runScript("detach(data)");	//afterwards we make sure it is detached to avoid superfluous messages and possible clobbering of analyses

	jaspRCPP_setErrorMsg(ColumnEncoder::decodeAll(jaspRCPP_getLastErrorMsg()).c_str());

	computedColumnFilter = "";

	return result;
}

//Isn't used anywhere at the moment but is meant to be called from jaspRCPP that is why const char * instead of std::string
bool rbridge_rCodeSafe(const char * rCode)
{
	std::string rCode64("local({" +rbridge_encodeColumnNamesInScript(rCode) + "})");

	try							{ R_FunctionWhiteList::scriptIsSafe(rCode64); }
	catch(filterException & e)	{ return false;	}

	return true;
}

void rbridge_setLANG(const std::string & lang)
{
	jaspRCPP_evalRCode(("Sys.setenv(LANG='" + lang + "');\nSys.setenv(LANGUAGE='" + lang + "');\nprint(Sys.getlocale());").c_str(), false);
}

extern "C" const char *	 STDCALL rbridge_system(const char * cmd)
{
	static std::string storage;

	storage = _system(cmd);

	return storage.c_str();
}

extern "C" void STDCALL rbridge_moduleLibraryFixer(const char * moduleLibrary)
{
	_moduleLibraryFixer(moduleLibrary);
}

extern "C" const char ** STDCALL rbridge_allColumnNames(size_t & numCols, bool encoded)
{
	static std::vector<std::string> cols;
	static const char **			names = nullptr;
	
	if(names)	free(names);
	
	cols	= encoded ? ColumnEncoder::columnNamesEncoded() : ColumnEncoder::columnNames();
	numCols	= cols.size();
	names	= static_cast<const char **>(malloc(sizeof(char*) * cols.size()));
	
	for(size_t i=0; i<numCols; i++)
		names[i] = cols[i].c_str();
	
	return names;
}


extern "C" const char * STDCALL rbridge_computedColumnFilterIs()
{
	return computedColumnFilter.c_str();
}
