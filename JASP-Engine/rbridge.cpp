//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include "base64.h"
#include "jsonredirect.h"
#include "sharedmemory.h"
#include "appinfo.h"
#include "tempfiles.h"
#include <iostream>

using namespace std;

DataSet *rbridge_dataSet = NULL;
RCallback rbridge_callback = NULL;
boost::function<void(const std::string &, std::string &, std::string &)> rbridge_fileNameSource = NULL;
boost::function<void(std::string &, std::string &)> rbridge_stateFileSource = NULL;
boost::function<DataSet *()> rbridge_dataSetSource = NULL;
std::unordered_set<std::string> filterColumnsUsed, columnNamesInDataSet;

char** rbridge_getLabels(const Labels &levels, int &nbLevels);
char** rbridge_getLabels(const vector<string> &levels, int &nbLevels);

void rbridge_init()
{
	RBridgeCallBacks callbacks = {
		rbridge_readDataSet,
		rbridge_readDataColumnNames,
		rbridge_readDataSetDescription,
		rbridge_requestStateFileSource,
		rbridge_requestTempFileName,
		rbridge_requestTempRootName,
		rbridge_runCallback,
		rbridge_readFullDataSet,
		rbridge_readDataSetForFiltering
	};

	jaspRCPP_init(AppInfo::getBuildYear().c_str(), AppInfo::version.asString().c_str(), &callbacks);
}

void rbridge_setDataSetSource(boost::function<DataSet* ()> source)
{
	rbridge_dataSetSource = source;
}

void rbridge_setFileNameSource(boost::function<void (const string &, string &, string &)> source)
{
	rbridge_fileNameSource = source;
}

void rbridge_setStateFileSource(boost::function<void (string &, string &)> source)
{
	rbridge_stateFileSource = source;
}

extern "C" bool STDCALL rbridge_requestStateFileSource(const char** root, const char **relativePath)
{
	if (!rbridge_stateFileSource)
		return false;

	static string _root;
	static string _relativePath;

	rbridge_stateFileSource(_root, _relativePath);

	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}

extern "C" bool STDCALL rbridge_requestTempFileName(const char* extensionAsString, const char** root, const char** relativePath)
{
	if (!rbridge_fileNameSource)
		return false;

	static string _root, _relativePath;

	rbridge_fileNameSource(extensionAsString, _root, _relativePath);
	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}

extern "C" const char* STDCALL rbridge_requestTempRootName()
{
	static string _root;
	_root = tempfiles_sessionDirName();

	return _root.c_str();
}

extern "C" bool STDCALL rbridge_runCallback(const char* in, int progress, const char** out)
{
	if (!rbridge_callback)
		return false;

	static string staticOut;
	staticOut = rbridge_callback(in, progress);
	*out = staticOut.c_str();

	return true;
}

string rbridge_run(const string &name, const string &title, bool &requiresInit, const string &dataKey, const string &options, const string &resultsMeta, const string &stateKey, const string &perform, int ppi, RCallback callback)
{
	rbridge_callback = callback;
	if (rbridge_dataSet != NULL) {
		rbridge_dataSet = rbridge_dataSetSource();
	}


	const char* results = jaspRCPP_run(name.c_str(), title.c_str(), requiresInit, dataKey.c_str(), options.c_str(), resultsMeta.c_str(), stateKey.c_str(), perform.c_str(), ppi);
	rbridge_callback = NULL;
	string str = results;

	return str;
}

extern "C" RBridgeColumn* STDCALL rbridge_readFullDataSet(int * colMax)
{
	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	if(rbridge_dataSet == NULL)
		return NULL;

	Columns &columns = rbridge_dataSet->columns();

	(*colMax) = columns.size();
	RBridgeColumnType* colHeaders = (RBridgeColumnType*)calloc((*colMax), sizeof(RBridgeColumnType));

	for(int i=0; i<(*colMax); i++)
	{
		colHeaders[i].name = strdup(columns[i].name().c_str());
		colHeaders[i].type = (int)columns[i].columnType();
	}

	RBridgeColumn * returnThis = rbridge_readDataSet(colHeaders, (*colMax), false);

	for(int i=0; i<(*colMax); i++)
		free(colHeaders[i].name);

	free(colHeaders);

	return returnThis;
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetForFiltering(int * colMax)
{
	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	Columns &columns = rbridge_dataSet->columns();

	(*colMax) = filterColumnsUsed.size();
	RBridgeColumnType* colHeaders = (RBridgeColumnType*)calloc((*colMax), sizeof(RBridgeColumnType));

	for(size_t iIn=0, iOut=0; iIn < columns.size() && iOut < filterColumnsUsed.size(); iIn++)
		if(filterColumnsUsed.count(columns[iIn].name()) > 0)
		{
			colHeaders[iOut].name = strdup(columns[iIn].name().c_str());
			colHeaders[iOut].type = (int)columns[iIn].columnType();

			iOut++;
		}


	RBridgeColumn * returnThis = rbridge_readDataSet(colHeaders, (*colMax), false);

	for(int i=0; i<(*colMax); i++)
		free(colHeaders[i].name);
	free(colHeaders);

	return returnThis;
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSet(RBridgeColumnType* colHeaders, int colMax, bool obeyFilter)
{
	if (colHeaders == NULL)
		return NULL;

	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	Columns &columns = rbridge_dataSet->columns();

	static RBridgeColumn* resultCols = NULL;
	static int lastColMax = 0;
	if (resultCols)
		freeRBridgeColumns(resultCols, lastColMax);
	lastColMax = colMax;
	resultCols = (RBridgeColumn*)calloc(colMax + 1, sizeof(RBridgeColumn));

	int filteredRowCount = obeyFilter ? rbridge_dataSet->filteredRowCount() : rbridge_dataSet->rowCount();

	// lets make some rownumbers/names for R that takes into account being filtered or not!
	resultCols[colMax].ints		= (int*)calloc(filteredRowCount, sizeof(int));
	resultCols[colMax].nbRows	= filteredRowCount;
	int filteredRow				= 0;
	for(int i=0; i<rbridge_dataSet->rowCount(); i++)
		if(rbridge_dataSet->filterVector()[i])
			resultCols[colMax].ints[filteredRow++] = i + 1; //R needs 1-based index


	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType& columnInfo = colHeaders[colNo];
		RBridgeColumn& resultCol = resultCols[colNo];

		string columnName = columnInfo.name;
		resultCol.name = strdup(Base64::encode("X", columnName, Base64::RVarEncoding).c_str());

		Column &column = columns.get(columnName);
		Column::ColumnType columnType = column.columnType();

		Column::ColumnType requestedType = (Column::ColumnType)columnInfo.type;
		if (requestedType == Column::ColumnTypeUnknown)
			requestedType = columnType;

		//int rowCount = column.rowCount();
		resultCol.nbRows = filteredRowCount;
		int rowNo = 0, dataSetRowNo = 0;

		if (requestedType == Column::ColumnTypeScale)
		{
			if (columnType == Column::ColumnTypeScale)
			{
				resultCol.isScale	= true;
				resultCol.hasLabels	= false;
				resultCol.doubles	= (double*)calloc(filteredRowCount, sizeof(double));

				for(double value : column.AsDoubles)
					if(!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++])
						resultCol.doubles[rowNo++] = value;
			}
			else if (columnType == Column::ColumnTypeOrdinal || columnType == Column::ColumnTypeNominal)
			{
				resultCol.isScale	= false;
				resultCol.hasLabels	= false;
				resultCol.ints		= (int*)calloc(filteredRowCount, sizeof(int));

				for(int value : column.AsInts)
					if(!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++])
						resultCol.ints[rowNo++] = value;
			}
			else // columnType == Column::ColumnTypeNominalText
			{
				resultCol.isScale	= false;
				resultCol.hasLabels = true;
				resultCol.isOrdinal = false;
				resultCol.ints		= (int*)calloc(filteredRowCount, sizeof(int));

				for(int value : column.AsInts)
					if(!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++])
					{
						if (value == INT_MIN)	resultCol.ints[rowNo++] = INT_MIN;
						else					resultCol.ints[rowNo++] = value + 1;
					}

				resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
			}
		}
		else // if (requestedType != Column::ColumnTypeScale)
		{
			resultCol.isScale	= false;
			resultCol.hasLabels	= true;
			resultCol.ints		= (int*)calloc(filteredRowCount, sizeof(int));
			resultCol.isOrdinal = (requestedType == Column::ColumnTypeOrdinal);

			if (columnType != Column::ColumnTypeScale)
			{
				std::map<int, int> indices;
				int i = 1; // R starts indices from 1

				const Labels &labels = column.labels();

				for(const Label &label : labels)
					indices[label.value()] = i++;

				for(int value : column.AsInts)
					if(!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++])
					{
						if (value == INT_MIN)	resultCol.ints[rowNo++] = INT_MIN;
						else					resultCol.ints[rowNo++] = indices.at(value);
					}

				resultCol.labels = rbridge_getLabels(labels, resultCol.nbLabels);
			}
			else
			{
				// scale to nominal or ordinal (doesn't really make sense, but we have to do something)
				resultCol.isScale	= false;
				resultCol.hasLabels = true;
				resultCol.isOrdinal = false;
				resultCol.ints		= (int*)calloc(filteredRowCount, sizeof(int));

				set<int> uniqueValues;

				for(double value : column.AsDoubles)
				{

					if (std::isnan(value))
						continue;

					int intValue;

					if (isfinite(value))	intValue = (int)(value * 1000);
					else if (value < 0)		intValue = INT_MIN;
					else					intValue = INT_MAX;

					uniqueValues.insert(intValue);
				}

				int index = 0;
				map<int, int> valueToIndex;
				vector<string> labels;

				for(int value : uniqueValues)
				{
					valueToIndex[value] = index++;

					if (value == INT_MAX)		labels.push_back("Inf");
					else if (value == INT_MIN)	labels.push_back("-Inf");
					else
					{
						stringstream ss;
						ss << ((double)value / 1000);
						labels.push_back(ss.str());
					}
				}

				for(double value : column.AsDoubles)
					if(!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++])
					{

						if (std::isnan(value))		resultCol.ints[rowNo] = INT_MIN;
						else if (isfinite(value))	resultCol.ints[rowNo] = valueToIndex[(int)(value * 1000)] + 1;
						else if (value > 0)			resultCol.ints[rowNo] = valueToIndex[INT_MAX] + 1;
						else						resultCol.ints[rowNo] = valueToIndex[INT_MIN] + 1;

						rowNo++;
					}

				resultCol.labels = rbridge_getLabels(labels, resultCol.nbLabels);
			}
		}
	}

	return resultCols;
}

extern "C" char** STDCALL rbridge_readDataColumnNames(int *colMax)
{
	if (rbridge_dataSet == NULL)
			rbridge_dataSet = rbridge_dataSetSource();

	Columns &columns = rbridge_dataSet->columns();
	static int staticColMax = 0;
	static char** staticResult = NULL;
	if (staticResult)
	{
		for (int i = 0; i < staticColMax; i++)
			free(staticResult[i]);
		free(staticResult);
	}
	staticColMax = rbridge_dataSet->columnCount();
	staticResult = (char**)calloc(staticColMax, sizeof(char*));

	int colNo = 0;
	for (const Column &column: columns)
		staticResult[colNo++] = strdup(column.name().c_str());

	*colMax = staticColMax;
	return staticResult;
}

extern "C" RBridgeColumnDescription* STDCALL rbridge_readDataSetDescription(RBridgeColumnType* columnsType, int colMax)
{
	if (!columnsType)
		return NULL;

	static int lastColMax = 0;
	static RBridgeColumnDescription* resultCols = NULL;
	if (resultCols)
		freeRBridgeColumnDescription(resultCols, lastColMax);
	lastColMax = colMax;
	resultCols = (RBridgeColumnDescription*)calloc(colMax, sizeof(RBridgeColumnDescription));

	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	Columns &columns = rbridge_dataSet->columns();

	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType& columnInfo = columnsType[colNo];
		RBridgeColumnDescription& resultCol = resultCols[colNo];

		string columnName = columnInfo.name;
		resultCol.name = strdup(Base64::encode("X", columnName, Base64::RVarEncoding).c_str());

		Column &column = columns.get(columnName);
		Column::ColumnType columnType = column.columnType();

		Column::ColumnType requestedType = (Column::ColumnType)columnInfo.type;

		if (requestedType == Column::ColumnTypeUnknown)
			requestedType = columnType;

		if (requestedType == Column::ColumnTypeScale)
		{
			if (columnType == Column::ColumnTypeScale)
			{
				resultCol.isScale = true;
				resultCol.hasLabels = false;
			}
			else if (columnType == Column::ColumnTypeOrdinal || columnType == Column::ColumnTypeNominal)
			{
				resultCol.isScale = false;
				resultCol.hasLabels = false;
			}
			else
			{
				resultCol.isScale = false;
				resultCol.hasLabels = true;
				resultCol.isOrdinal = false;
				resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
			}
		}
		else
		{
			resultCol.isScale = false;
			resultCol.hasLabels = true;
			resultCol.isOrdinal = (requestedType == Column::ColumnTypeOrdinal);
			if (columnType != Column::ColumnTypeScale)
			{
				resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
			}
			else
			{
				// scale to nominal or ordinal (doesn't really make sense, but we have to do something)
				set<int> uniqueValues;

				for (double value: column.AsDoubles)
				{
					if (std::isnan(value))
						continue;

					int intValue;

					if (isfinite(value))
						intValue = (int)(value * 1000);
					else if (value < 0)
						intValue = INT_MIN;
					else
						intValue = INT_MAX;

					uniqueValues.insert(intValue);
				}

				vector<string> labels;

				for (int value: uniqueValues)
				{
					if (value == INT_MAX)
					{
						labels.push_back("Inf");
					}
					else if (value == INT_MIN)
					{
						labels.push_back("-Inf");
					}
					else
					{
						stringstream ss;
						ss << ((double)value / 1000);
						labels.push_back(ss.str());
					}
				}

				resultCol.labels = rbridge_getLabels(labels, resultCol.nbLabels);

			}

		}
	}

	return resultCols;
}

void freeRBridgeColumns(RBridgeColumn *columns, int colMax)
{
	for (int i = 0; i < colMax; i++)
	{
		RBridgeColumn& column = columns[i];
		free(column.name);
		if (column.isScale)
			free(column.doubles);
		else
			free(column.ints);
		if (column.hasLabels)
			freeLabels(column.labels, column.nbLabels);
	}
	free(columns[colMax].ints); //rownames/numbers
	free(columns);
}

void freeRBridgeColumnDescription(RBridgeColumnDescription* columns, int colMax)
{
	for (int i = 0; i < colMax; i++)
	{
		RBridgeColumnDescription& column = columns[i];
		free(column.name);
		if (column.hasLabels)
			freeLabels(column.labels, column.nbLabels);
	}
	free(columns);
}

void freeLabels(char** labels, int nbLabels)
{
	for (int i = 0; i < nbLabels; i++)
		free(labels[i]);
	free(labels);
}

char** rbridge_getLabels(const Labels &levels, int &nbLevels)
{
	char** results = NULL;
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
		for (const Label &level: levels)
			results[i++] = strdup(level.text().c_str());
		nbLevels = i;
	}

	return results;
}

char** rbridge_getLabels(const vector<string> &levels, int &nbLevels)
{
	char** results = NULL;
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
		for (const string &level: levels)
			results[i++] = strdup(level.c_str());
		nbLevels = i;
	}

	return results;
}


string rbridge_check()
{
	return jaspRCPP_check();
}



bool rbridge_columnUsedInFilter(const char * columnName)
{
	return filterColumnsUsed.count(std::string(columnName)) > 0;
}


std::string	rbridge_encodeColumnNamesToBase64(std::string & filterCode)
{
	//std::cout << " rbridge_encodeColumnNamesToBase64 starts with: "<<filterCode << std::endl << std::flush;

	std::string filterBase64 = filterCode;

	rbridge_findColumnsUsedInDataSet();
	filterColumnsUsed.clear();

	static std::regex nonNameChar("[^\\.A-Za-z0-9]");

	//for now we simply replace any found columnname by its Base64 variant if found
	size_t foundPos = -1;
	for(std::string col : columnNamesInDataSet)
	{
		std::string columnName64 = Base64::encode("X", col, Base64::RVarEncoding);

		while((foundPos = filterBase64.find(col, foundPos + 1)) != std::string::npos)
		{
			size_t foundPosEnd = foundPos + col.length();
			//First check if it is a "free columnname" aka is there some space or a kind in front of it. We would not want to replace a part of another term (Imagine what happens when you use a columname such as "E" and a filter that includes the term TRUE, it does not end well..)
			bool startIsFree	= foundPos == 0							|| std::regex_match(filterBase64.substr(foundPos - 1, 1),	nonNameChar);
			bool endIsFree		= foundPosEnd == filterBase64.length()	|| (std::regex_match(filterBase64.substr(foundPosEnd, 1),	nonNameChar) && filterBase64[foundPosEnd] != '('); //Check for "(" as well because maybe someone has a columnname such as rep or if or something weird like that

			if(startIsFree && endIsFree)
			{
				filterBase64.replace(foundPos, col.length(), columnName64);

				if(filterColumnsUsed.count(col) == 0)
					filterColumnsUsed.insert(col);
			}
		}
	}

	//std::cout << " rbridge_encodeColumnNamesToBase64 results in: "<<filterBase64 << std::endl << std::flush;

	return filterBase64;
}

std::string	rbridge_decodeColumnNamesFromBase64(std::string messageBase64)
{
	std::string messageNormal = messageBase64;

	rbridge_findColumnsUsedInDataSet();

	//for now we simply replace any found columnname in its Base64 variant by its normal version
	size_t foundPos = 0;
	for(std::string columnName : columnNamesInDataSet)
	{
		std::string col64 = Base64::encode("X", columnName, Base64::RVarEncoding);

		while((foundPos = messageNormal.find(col64, 0)) != std::string::npos)
			messageNormal.replace(foundPos, col64.length(), columnName);

	}

	return messageNormal;
}

void rbridge_findColumnsUsedInDataSet()
{
	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	Columns &columns = rbridge_dataSet->columns();

	columnNamesInDataSet.clear();

	for(Column & col : columns)
		columnNamesInDataSet.insert(col.name());
}

std::vector<bool> rbridge_applyFilter(std::string & filterCode, std::string & generatedFilterCode)
{
	rbridge_dataSet = rbridge_dataSetSource();

	size_t rowCount = rbridge_dataSet->rowCount();

	if(filterCode == "*" || filterCode == "") //if * then there is no filter so everything is fine :)
		return std::vector<bool>(rowCount, true);

	static std::string errorMsg;

	R_FunctionWhiteList::scriptIsSafe(filterCode); //can throw filterExceptions

	std::string concatenated = generatedFilterCode + "\n" + filterCode, filter64(rbridge_encodeColumnNamesToBase64(concatenated));

	bool * arrayPointer = NULL;

	jaspRCPP_runScript("data <- .readFilterDatasetToEnd();\nattach(data);\noptions(warn=1, showWarnCalls=TRUE, showErrorCalls=TRUE, show.error.messages=TRUE)"); //first we load the data to be filtered
	int arrayLength	= jaspRCPP_runFilter(filter64.c_str(), &arrayPointer);
	jaspRCPP_runScript("detach(data)");	//and afterwards we make sure it is detached to avoid superfluous messages and possible clobbering of analyses

	if(arrayLength < 0)
	{
		errorMsg = rbridge_decodeColumnNamesFromBase64(jaspRCPP_getLastFilterErrorMsg());
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
		throw filterException("Filtered out all data..");

	if(arrayLength != rowCount)
	{
		std::stringstream msg;
		msg << "Filter did not return a logical vector of length " << rowCount << " as expected, instead it returned a logical vector of length " << arrayLength << std::endl << std::flush;
		errorMsg = msg.str();
		throw filterException(errorMsg);
	}

	return returnThis;
}

string rbridge_saveImage(const string &name, const string &type, const int &height, const int &width, const int ppi)
{
	return jaspRCPP_saveImage(name.c_str(), type.c_str(), height, width, ppi);
}

string rbridge_editImage(const string &name, const string &type, const int &height, const int &width, const int ppi)
{
	return jaspRCPP_editImage(name.c_str(), type.c_str(), height, width, ppi);
}

string rbridge_evalRCode(const string &rCode)
{
	return jaspRCPP_evalRCode(rCode.c_str());
}
