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

#include <boost/foreach.hpp>

#include "base64.h"
#include "jsonredirect.h"
#include "sharedmemory.h"
#include "appinfo.h"

using namespace std;

DataSet *rbridge_dataSet;
RCallback rbridge_callback;
boost::function<void(const std::string &, std::string &, std::string &)> rbridge_fileNameSource;
boost::function<void(std::string &, std::string &)> rbridge_stateFileSource;
boost::function<DataSet *()> rbridge_dataSetSource;
std::unordered_set<std::string> filterColumnsUsed, columnNamesInDataSet;


char** rbridge_getLabels(const Labels &levels, int &nbLevels);
char** rbridge_getLabels(const vector<string> &levels, int &nbLevels);

void rbridge_init()
{
	rbridge_dataSet = NULL;
	rbridge_callback = NULL;
	rbridge_fileNameSource = NULL;
	rbridge_stateFileSource = NULL;

	RBridgeCallBacks callbacks = {
		rbridge_readDataSet,
		rbridge_readDataColumnNames,
		rbridge_readDataSetDescription,
		rbridge_requestStateFileSource,
		rbridge_requestTempFileName,
		rbridge_runCallback,
		rbridge_readFullDataSet,
		rbridge_readFilterDataSet
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
	if (!rbridge_stateFileSource)
		return false;

	static string _root, _relativePath;

	rbridge_fileNameSource(extensionAsString, _root, _relativePath);
	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
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

extern "C" RBridgeColumn* STDCALL rbridge_readFilterDataSet(int * colMax)
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
	resultCols = (RBridgeColumn*)calloc(colMax, sizeof(RBridgeColumn));

	int filteredRowCount = obeyFilter ? rbridge_dataSet->filteredRowCount() : rbridge_dataSet->rowCount();

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
	BOOST_FOREACH(const Column &column, columns)
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
			bool ordinal = (requestedType == Column::ColumnTypeOrdinal);

			resultCol.isScale = false;
			resultCol.hasLabels = true;
			resultCol.isOrdinal = ordinal;
			resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
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
		BOOST_FOREACH(const Label &level, levels)
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
		BOOST_FOREACH(const string &level, levels)
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
	std::string filterBase64 = filterCode;

	std::cout << "Scanning filter for columnNames: " << filterCode << "\n" << std::flush;

	rbridge_findColumnsUsedInDataSet();
	filterColumnsUsed.clear();

	//for now we only look for dataset$ 'cause its easy.
	std::string searchThis("dataset$");
	size_t startPos = 0, foundPos = 0;
	while((foundPos = filterBase64.find(searchThis, startPos)) != std::string::npos)
	{
		size_t colNameStart	= foundPos + searchThis.length();
		size_t colNameEnd	= filterBase64.find_first_of(" \t\n\r", colNameStart);
		size_t colNameLen	= (colNameEnd == std::string::npos ? filterBase64.length() : colNameEnd) - colNameStart;

		std::string foundColumnName(filterBase64.substr(colNameStart, colNameLen));

		if(columnNamesInDataSet.count(foundColumnName) == 0)
			throw filterException(std::string("filter contains unknown columnName: ") + foundColumnName);

		if(filterColumnsUsed.count(foundColumnName) == 0)
			filterColumnsUsed.insert(foundColumnName);

		//Now we replace the foundColumnName by something Base64.
		std::string columnName64 = Base64::encode("X", foundColumnName, Base64::RVarEncoding);
		filterBase64.replace(colNameStart, colNameLen, columnName64);
		startPos = colNameStart + columnName64.length();

		std::cout << "found " << foundColumnName << " and replaced it by " << columnName64 << "\n" << std::flush;
	}

	std::cout << "Resulting filter is: " << filterBase64 << "\n" << std::flush;

	return filterBase64;

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

std::vector<bool> rbridge_applyFilter(std::string & filterCode)
{
	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	if(filterCode == "*") //if * then there is no filter so everything is true :)
	{
		size_t rowCount = rbridge_dataSet->rowCount();
		return std::vector<bool>(rowCount, true);
	}

	std::string filterWithBoilerPlate("dataset <- .readFilterDatasetToEnd()\n.returnDataFrame(colnames(dataset))\n.returnDataFrame(dataset)\n");
	filterWithBoilerPlate += rbridge_encodeColumnNamesToBase64(filterCode);

	try
	{
		bool * arrayPointer = NULL;
		std::cout << "Gonna try filter \"" << filterWithBoilerPlate <<"\"!\n" << std::flush;

		int arrayLength		= jaspRCPP_runFilter(filterWithBoilerPlate.c_str(), &arrayPointer);

		std::cout << "That didnt crash apparently and the result is: " << arrayLength << "\n" << std::flush;
		std::vector<bool> returnThis;

		if(arrayLength > 0)
		{
			for(int i=0; i<arrayLength; i++)
				returnThis.push_back(arrayPointer[i]);

			free(arrayPointer);
		}

		return returnThis;
	}
	catch(std::exception & e)
	{
		std::cout << "Something went wrong with rbridge_applyFilter, namely: " << e.what() << "\n" << std::flush;
		throw e;
	}
}

string rbridge_saveImage(const string &name, const string &type, const int &height, const int &width, const int ppi)
{
	return jaspRCPP_saveImage(name.c_str(), type.c_str(), height, width, ppi);
}
