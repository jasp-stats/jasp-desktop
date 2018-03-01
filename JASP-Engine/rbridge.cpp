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
		rbridge_runCallback
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

	const char* results = jaspRCPP_run(name.c_str(), title.c_str(), requiresInit, dataKey.c_str(), options.c_str(), resultsMeta.c_str(), stateKey.c_str(), perform.c_str(), ppi);
	rbridge_callback = NULL;
	string str = results;

	return str;
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSet(RBridgeColumnType* colHeaders, int colMax)
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

		int rowCount = column.rowCount();
		resultCol.nbRows = rowCount;
		int rowNo = 0;

		if (requestedType == Column::ColumnTypeScale)
		{
			if (columnType == Column::ColumnTypeScale)
			{
				resultCol.isScale = true;
				resultCol.hasLabels = false;
				resultCol.doubles = (double*)calloc(rowCount, sizeof(double));

				for (double value: column.AsDoubles)
				{
					resultCol.doubles[rowNo++] = value;
				}

			}
			else if (columnType == Column::ColumnTypeOrdinal || columnType == Column::ColumnTypeNominal)
			{
				resultCol.isScale = false;
				resultCol.hasLabels = false;
				resultCol.ints = (int*)calloc(rowCount, sizeof(int));

				for (int value: column.AsInts)
				{
					resultCol.ints[rowNo++] = value;
				}
			}
			else // columnType == Column::ColumnTypeNominalText
			{
				resultCol.isScale = false;
				resultCol.hasLabels = true;
				resultCol.isOrdinal = false;
				resultCol.ints = (int*)calloc(rowCount, sizeof(int));

				for (int value: column.AsInts)
				{
					if (value == INT_MIN)
						resultCol.ints[rowNo++] = INT_MIN;
					else
						resultCol.ints[rowNo++] = value + 1;
				}

				resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
			}
		}
		else // if (requestedType != Column::ColumnTypeScale)
		{
			resultCol.isScale = false;
			resultCol.hasLabels = true;
			resultCol.ints = (int*)calloc(rowCount, sizeof(int));
			resultCol.isOrdinal = (requestedType == Column::ColumnTypeOrdinal);

			if (columnType != Column::ColumnTypeScale)
			{
				std::map<int, int> indices;
				int i = 1; // R starts indices from 1

				const Labels &labels = column.labels();

				for (const Label &label: labels)
				{
					indices[label.value()] = i++;
				}

				for (int value: column.AsInts)
				{
					if (value == INT_MIN)
						resultCol.ints[rowNo++] = INT_MIN;
					else
						resultCol.ints[rowNo++] = indices.at(value);
				}

				resultCol.labels = rbridge_getLabels(labels, resultCol.nbLabels);
			}
			else
			{
				// scale to nominal or ordinal (doesn't really make sense, but we have to do something)
				resultCol.isScale = false;
				resultCol.hasLabels = true;
				resultCol.isOrdinal = false;
				resultCol.ints = (int*)calloc(rowCount, sizeof(int));
				
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

				int index = 0;
				map<int, int> valueToIndex;
				vector<string> labels;

				for (int value: uniqueValues)
				{
					valueToIndex[value] = index;

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

					index++;
				}

				for (double value: column.AsDoubles)
				{
					if (std::isnan(value))
						resultCol.ints[rowNo] = INT_MIN;
					else if (isfinite(value))
						resultCol.ints[rowNo] = valueToIndex[(int)(value * 1000)] + 1;
					else if (value > 0)
						resultCol.ints[rowNo] = valueToIndex[INT_MAX] + 1;
					else
						resultCol.ints[rowNo] = valueToIndex[INT_MIN] + 1;

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

string rbridge_saveImage(const string &name, const string &type, const int &height, const int &width, const int ppi)
{
	return jaspRCPP_saveImage(name.c_str(), type.c_str(), height, width, ppi);
}

string rbridge_editImage(const string &name, const string &type, const int &height, const int &width, const int ppi)
{
	return jaspRCPP_editImage(name.c_str(), type.c_str(), height, width, ppi);
}
