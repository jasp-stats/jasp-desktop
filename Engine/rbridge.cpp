//
// Copyright (C) 2013-2024 University of Amsterdam
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

#include "rbridge.h"
#include <json/json.h>
#include "dataset.h"
#include "appinfo.h"
#include "tempfiles.h"
#include "log.h"
#include "timers.h"
#include "r_functionwhitelist.h"
#include "otoolstuff.h"
#include "enginebase.h"
#include "r_functionwhitelist.h"
#include <sstream>

#ifdef _WIN32
#include <windows.h>
#endif

EngineBase					*	rbridge_engine		= nullptr;
DataSet						*	rbridge_dataSet		= nullptr;
RCallback						rbridge_callback	= NULL;
std::set<std::string>			filterColumnsUsed;
std::vector<std::string>		columnNamesInDataSet;
ColumnEncoder				*	extraEncodings		= nullptr;
ColumnEncoder::colsPlusTypes	datasetWanted;

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

void rbridge_setEngine(EngineBase * engine)
{
	rbridge_engine = engine;
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

void rbridge_init(EngineBase * engine, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction, ColumnEncoder * extraEncoder, const char * resultFont)
{
	JASPTIMER_SCOPE(rbridge_init);
	
	rbridge_setEngine(engine);
	
	Log::log() << "Setting extraEncodings." << std::endl;
	extraEncodings = extraEncoder;

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
		rbridge_requestJaspResultsFileSource,
		rbridge_getColumnType,
		rbridge_createColumn,
		rbridge_deleteColumn,
		rbridge_getColumnAnalysisId,
		rbridge_setColumnDataAndType,
		rbridge_dataSetRowCount,
		rbridge_encodeColumnName,
		rbridge_decodeColumnName,
		rbridge_encodeAllColumnNames,
		rbridge_decodeAllColumnNames,
		rbridge_decodeColumnType,
		rbridge_shouldEncodeColumnName,
		rbridge_shouldDecodeColumnName,
		rbridge_allColumnNames
	};

	JASPTIMER_START(jaspRCPP_init);

	static std::string tempDirStatic = TempFiles::createTmpFolder();
	static std::string initRCode	 = jaspBaseDistributionSamplersR + "\n" + jaspBaseFriendlyConstructorFunctionsR + "\n" + jaspBaseTransformFunctionsR;
	
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
					initRCode.c_str()
	);
	JASPTIMER_STOP(jaspRCPP_init);

}

void rbridge_junctionHelper(bool collectNotRestore, const std::string & modulesFolder, const std::string& linkFolder, const std::string& junctionFilePath)
{
	jaspRCPP_junctionHelper(collectNotRestore, modulesFolder.c_str(), linkFolder.c_str(), junctionFilePath.c_str());
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
	static std::string out;

	if(extraEncodings->shouldDecode(in))	out = extraEncodings->decode(in);
	else									out = ColumnEncoder::columnEncoder()->decode(in);

	return out.c_str();
}

extern "C" int STDCALL rbridge_decodeColumnType(const char * in)
{
	static columnType colType;

	if(extraEncodings->shouldDecode(in))	colType = extraEncodings->columnTypeFromEncoded(in);
	else									colType = ColumnEncoder::columnEncoder()->columnTypeFromEncoded(in);

	return int(colType);
}

extern "C" bool STDCALL rbridge_shouldEncodeColumnName(const char * in)
{
	return ColumnEncoder::columnEncoder()->shouldEncode(in);
}

extern "C" bool STDCALL rbridge_shouldDecodeColumnName(const char * in)
{
	return ColumnEncoder::columnEncoder()->shouldDecode(in);
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
	out = ColumnEncoder::columnEncoder()->decodeAll(in);
	return out.c_str();
}

extern "C" bool STDCALL rbridge_requestJaspResultsFileSource(const char** root, const char **relativePath)
{
	if (!rbridge_engine)
		return false;

	static std::string _root;
	static std::string _relativePath;

	rbridge_engine->provideJaspResultsFileName(_root, _relativePath);

	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}


extern "C" bool STDCALL rbridge_requestStateFileSource(const char** root, const char **relativePath)
{
	if (!rbridge_engine)
		return false;

	static std::string _root;
	static std::string _relativePath;

	rbridge_engine->provideStateFileName(_root, _relativePath);

	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}

extern "C" bool STDCALL rbridge_requestTempFileName(const char* extensionAsString, const char** root, const char** relativePath)
{
	if (!rbridge_engine)
		return false;

	static std::string _root, _relativePath;

	rbridge_engine->provideTempFileName(extensionAsString, _root, _relativePath);
	*root			= _root.c_str();
	*relativePath	= _relativePath.c_str();
	return true;
}

extern "C" bool STDCALL rbridge_requestSpecificFileName(const char* specificFilename, const char** root, const char** relativePath)
{
	if (!rbridge_engine)
		return false;

	static std::string _root, _relativePath, _specific;

	_specific = specificFilename;

	rbridge_engine->provideSpecificFileName(_specific, _root, _relativePath);

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

std::string rbridge_runModuleCall(const std::string &name, const std::string &title, const std::string &moduleCall, const std::string &dataKey, const std::string &options, const std::string &stateKey, int analysisID, int analysisRevision, bool developerMode, ColumnEncoder::colsPlusTypes datasetColsTypes, bool preloadData)
{
	rbridge_callback	= NULL; //Only jaspResults here so callback is not needed
	if (rbridge_dataSet != nullptr)
		rbridge_dataSet		= rbridge_engine->provideAndUpdateDataSet();
	
	datasetWanted = datasetColsTypes;

	return jaspRCPP_runModuleCall(name.c_str(), title.c_str(), moduleCall.c_str(), dataKey.c_str(), options.c_str(), stateKey.c_str(), analysisID, analysisRevision, developerMode, preloadData);
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
	rbridge_dataSet = rbridge_engine->provideAndUpdateDataSet();

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

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetForFiltering(size_t * colMax)
{
	rbridge_dataSet = rbridge_engine->provideAndUpdateDataSet();

	const Columns & columns = rbridge_dataSet->columns();

	datasetWanted.clear();

	for(const std::string & colName : filterColumnsUsed)
		if(rbridge_dataSet->column(colName))
			datasetWanted.insert(std::make_pair(colName, rbridge_dataSet->column(colName)->type()));
		else if(!colName.empty())
		{
			//It should then be a columnName + '.' + type
			//And decoding always goes to the original columnname so:
			std::string colNameSansType = ColumnEncoder::columnEncoder()->decode(	ColumnEncoder::columnEncoder()->encode(colName));

			assert(colName[colNameSansType.size()] == '.'); //Because how??? ;)

			std::string typeStr = colName.substr(colNameSansType.size() + 1);

			assert(columnTypeValidName(typeStr));

			datasetWanted.insert(std::make_pair(colName, columnTypeFromString(typeStr)));
		}

	return rbridge_readDataSetRequested(colMax, false);
}

static RBridgeColumn*	datasetStatic = nullptr;
static int				datasetColMax = 0;

extern "C" RBridgeColumn* STDCALL rbridge_readDataSet(RBridgeColumnType* colHeaders, size_t colMax, bool obeyFilter)
{
	if (colHeaders == nullptr)
		return nullptr;

	rbridge_dataSet = rbridge_engine->provideAndUpdateDataSet();

	if(rbridge_dataSet == nullptr)
		return nullptr;

	const Columns & columns = rbridge_dataSet->columns();

	if (datasetStatic != nullptr)
		freeRBridgeColumns();

	datasetColMax = colMax;
	datasetStatic = static_cast<RBridgeColumn*>(calloc(datasetColMax + 1, sizeof(RBridgeColumn)));

	size_t filteredRowCount = obeyFilter ? rbridge_dataSet->filter()->filteredRowCount() : rbridge_dataSet->rowCount();

	// lets make some rownumbers/names for R that takes into account being filtered or not!
	datasetStatic[colMax].ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));
	datasetStatic[colMax].nbRows	= filteredRowCount;
	int filteredRow					= 0;

	//If you change anything here, make sure that "label outliers" in Descriptives still works properly (including with filters)
	for(size_t i=0; i<rbridge_dataSet->rowCount() && filteredRow < datasetStatic[colMax].nbRows; i++)
		if(
				!obeyFilter ||
				(rbridge_dataSet->filter()->filtered().size() > i && rbridge_dataSet->filter()->filtered()[i])
			)
			datasetStatic[colMax].ints[filteredRow++] = int(i + 1); //R needs 1-based index

	//std::cout << "reading " << colMax << " columns!\nRowCount: " << filteredRowCount << "" << std::endl;

	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType	&	columnInfo		= colHeaders[colNo];
		RBridgeColumn		&	resultCol		= datasetStatic[colNo];
		std::string				columnName		= ColumnEncoder::columnEncoder()->decode(columnInfo.name);
								resultCol.name	= strdup(columnInfo.name);
		Column				*	column			= rbridge_dataSet->column(columnName);
		columnType				colType			= column->type(),
								requestedType	= columnType(columnInfo.type);

		if (requestedType == columnType::unknown)
			requestedType = colType;

		resultCol.nbRows = filteredRowCount;
		
		if (requestedType == columnType::scale)
		{
			int rowNo = 0;
					
			resultCol.isScale	= true;
			resultCol.doubles	= (double*)calloc(filteredRowCount, sizeof(double));
			
			boolvec filterToUse;
			if(obeyFilter)
				filterToUse = rbridge_dataSet->filter()->filtered();

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
				filterToUse = rbridge_dataSet->filter()->filtered();
			
			stringvec levels = column->dataAsRLevels(vals, filterToUse, true);
			
			for(size_t i=0; i<vals.size(); i++)
				resultCol.ints[i] = vals[i] == EmptyValues::missingValueInteger ? vals[i] : vals[i] + 1; //R chokes on 0-based indices

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

	free(columns);
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetRequested(size_t * colMax, bool obeyFilter)
{
	*colMax = 0;
	RBridgeColumnType * requestedColumns = new RBridgeColumnType[datasetWanted.size()];

    for(const auto & nameType : datasetWanted)
    {
		//Make sure this is encodable/decodable
		if(!ColumnEncoder::columnEncoder()->shouldEncode(nameType.first) && !ColumnEncoder::columnEncoder()->shouldDecode(nameType.first))
			throw std::runtime_error("rbridge_readDataSetRequested gets '" + nameType.first + "' but its not a columnname at all");
		
		requestedColumns[*colMax].name = strdup(ColumnEncoder::columnEncoder()->encodeAll(nameType.first).c_str());
        requestedColumns[*colMax].type = int(nameType.second);

        (*colMax)++;
    }

	RBridgeColumn * theData = rbridge_readDataSet(requestedColumns, *colMax, obeyFilter);

	__freeRBridgeColumnType(requestedColumns, *colMax);

	return theData;
}

extern "C" char** STDCALL rbridge_readDataColumnNames(size_t * colMax)
{
	rbridge_dataSet = rbridge_engine->provideAndUpdateDataSet();

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
	rbridge_dataSet			= rbridge_engine->provideAndUpdateDataSet();
	const Columns & columns	= rbridge_dataSet->columns();

	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType			&	columnInfo		= columnsType[colNo];
		RBridgeColumnDescription	&	resultCol		= resultCols[colNo];
		std::string						columnName		= ColumnEncoder::columnEncoder()->decode(columnInfo.name);
										resultCol.name	= strdup(columnInfo.name);
		Column						*	column			= rbridge_dataSet->column(columnName);
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

///Sneaky variable declaration of colName!
#define JASP_COLUMN_DECODE_HERE_STORED_colName std::string colName(ColumnEncoder::columnEncoder()->decode(columnName))

extern "C" int STDCALL rbridge_getColumnType(const char * columnName)
{
	if(!ColumnEncoder::columnEncoder()->shouldDecode(columnName))
		return rbridge_engine->getColumnType(columnName);

	JASP_COLUMN_DECODE_HERE_STORED_colName;
	return rbridge_engine->getColumnType(colName);
}

extern "C" int STDCALL rbridge_getColumnAnalysisId(const char * columnName)
{
	if(!ColumnEncoder::columnEncoder()->shouldDecode(columnName))
		return  rbridge_engine->getColumnAnalysisId(columnName);

	JASP_COLUMN_DECODE_HERE_STORED_colName;
	return rbridge_engine->getColumnAnalysisId(colName);
}

extern "C" const char * STDCALL rbridge_createColumn(const char * columnName)
{
	static std::string lastColumnName;
	lastColumnName = rbridge_engine->createColumn(columnName);

	return lastColumnName.c_str();
}

extern "C" bool STDCALL rbridge_deleteColumn(const char * columnName)
{
	return rbridge_engine->deleteColumn(columnName);
}

extern "C" bool STDCALL rbridge_setColumnDataAndType(const char* columnName, const char ** nominalData, size_t length, int _columnType)
{
	JASP_COLUMN_DECODE_HERE_STORED_colName;

	std::vector<std::string> nominals(nominalData, nominalData + length);

	return rbridge_engine->setColumnDataAndType(colName, nominals, columnType(_columnType));
}

extern "C" int	STDCALL rbridge_dataSetRowCount()
{
	return rbridge_engine->dataSetRowCount();
}

void rbridge_memoryCleaning()
{
	freeRBridgeColumns();
	jaspRCPP_purgeGlobalEnvironment();
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

std::string	rbridge_encodeColumnNamesInScript(const std::string & filterCode)
{
	return ColumnEncoder::columnEncoder()->encodeRScript(filterCode, &filterColumnsUsed);
}

void rbridge_setupRCodeEnv(int rowCount, const std::string & dataname)
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

std::vector<bool> rbridge_applyFilter(const std::string & filterCode, const std::string & generatedFilterCode)
{
	rbridge_dataSet = rbridge_engine->provideAndUpdateDataSet();

	if(rbridge_dataSet == nullptr)
		throw filterException("rbridge_dataSet == nullptr!");

	int rowCount = rbridge_dataSet->rowCount();

	if(filterCode == "*" || filterCode == "") //if * then there is no filter so everything is fine :)
		return std::vector<bool>(rowCount, true);

	static std::string errorMsg;

	std::string	concatenated = generatedFilterCode + "\n" + filterCode,
				filter64	 = "local({" + rbridge_encodeColumnNamesInScript(concatenated) + "})";

	R_FunctionWhiteList::scriptIsSafe(filter64); //can throw filterExceptions

	bool * arrayPointer = nullptr;

	rbridge_setupRCodeEnv(rowCount);
	int arrayLength	= jaspRCPP_runFilter(filter64.c_str(), &arrayPointer);
	rbridge_detachRCodeEnv();

	if(arrayLength < 0)
	{
		errorMsg = ColumnEncoder::columnEncoder()->decodeAll(jaspRCPP_getLastErrorMsg());

		if(errorMsg == "")
			errorMsg = "Filter returned something incomprehensible, make sure you entered all columnnames *exactly* right.";

		throw filterException(errorMsg.c_str());
	}

	std::vector<bool> returnThis;

	bool atLeastOneRow = false;
	if(arrayLength == rowCount) //Only build boolvector if it matches the desired length.
		for(int i=0; i<arrayLength; i++)
		{
			returnThis.push_back(arrayPointer[i]);
			if(arrayPointer[i])
				atLeastOneRow = true;
		}

	jaspRCPP_freeArrayPointer(&arrayPointer);

	if(!atLeastOneRow)
		throw filterException("Filtered out all data.");

	if(arrayLength != rowCount)
	{
		std::stringstream msg;
		msg << "Filter did not return a logical vector of length " << rowCount << " as expected, instead it returned a logical vector of length " << arrayLength << std::endl;
		errorMsg = msg.str();
		throw filterException(errorMsg);
	}

	return returnThis;
}

std::string rbridge_evalRCodeWhiteListed(const std::string & rCode, bool setWd)
{
	rbridge_dataSet = rbridge_engine->provideAndUpdateDataSet();
	int rowCount	= rbridge_dataSet == nullptr ? 0 : rbridge_dataSet->rowCount();

	jaspRCPP_resetErrorMsg();

	std::string rCode64("local({" +rbridge_encodeColumnNamesInScript(rCode) + "})");

	try							{ R_FunctionWhiteList::scriptIsSafe(rCode64); }
	catch(filterException & e)	{ jaspRCPP_setErrorMsg(e.what()); return std::string("R code is not safe because of: ") + e.what();	}


	rbridge_setupRCodeEnv(rowCount);
	std::string result = jaspRCPP_evalRCode(rCode64.c_str(), setWd);
	jaspRCPP_runScript("detach(data)");	//and afterwards we make sure it is detached to avoid superfluous messages and possible clobbering of analyses

	jaspRCPP_setErrorMsg(ColumnEncoder::columnEncoder()->decodeAll(jaspRCPP_getLastErrorMsg()).c_str());

	return result;
}


std::string rbridge_evalRComputedColumn(const std::string &rCode, const std::string & setColumnFunc)
{
	rbridge_dataSet = rbridge_engine->provideAndUpdateDataSet();
	int rowCount	= rbridge_dataSet == nullptr ? 0 : rbridge_dataSet->rowCount();

	jaspRCPP_resetErrorMsg();

	std::string rCode64(rbridge_encodeColumnNamesInScript(rCode));

	try							{ R_FunctionWhiteList::scriptIsSafe(rCode64); }
	catch(filterException & e)	{ jaspRCPP_setErrorMsg(e.what()); return std::string("R code is not safe because of: ") + e.what();	}


	rbridge_setupRCodeEnv(rowCount);
	std::string result = jaspRCPP_evalComputedColumn(rCode64.c_str(), setColumnFunc.c_str());
	jaspRCPP_runScript("detach(data)");	//and afterwards we make sure it is detached to avoid superfluous messages and possible clobbering of analyses

	jaspRCPP_setErrorMsg(ColumnEncoder::columnEncoder()->decodeAll(jaspRCPP_getLastErrorMsg()).c_str());

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

