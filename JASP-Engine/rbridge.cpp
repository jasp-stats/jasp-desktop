//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "../JASP-Common/base64.h"
#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/sharedmemory.h"
#include "../JASP-Common/appinfo.h"

RInside *rbridge_rinside;
DataSet *rbridge_dataSet;

using namespace std;

RCallback rbridge_runCallback;
boost::function<void(const std::string &, std::string &, std::string &)> rbridge_fileNameSource;
boost::function<void(std::string &, std::string &)> rbridge_stateFileSource;
boost::function<DataSet *()> rbridge_dataSetSource;

Rcpp::DataFrame rbridge_readDataSetSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
Rcpp::DataFrame rbridge_readDataSetHeaderSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
std::map<std::string, Column::ColumnType> rbridge_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
SEXP rbridge_callbackSEXP(SEXP results);
SEXP rbridge_requestTempFileNameSEXP(SEXP extension);
SEXP rbridge_requestStateFileNameSEXP();

SEXP rbridge_callback(SEXP results);
Rcpp::DataFrame rbridge_readDataSet(const std::map<std::string, Column::ColumnType> &columns);
Rcpp::DataFrame rbridge_readDataSetHeader(const std::map<std::string, Column::ColumnType> &columns);

void rbridge_makeFactor(Rcpp::IntegerVector &v, const std::vector<std::string> &levels, bool ordinal = false);
void rbridge_makeFactor(Rcpp::IntegerVector &v, const Labels &levels, bool ordinal = false);


void rbridge_init()
{
	rbridge_dataSet = NULL;
	rbridge_runCallback = NULL;
	rbridge_fileNameSource = NULL;
	rbridge_stateFileSource = NULL;

	rbridge_rinside = new RInside();

	RInside &rInside = rbridge_rinside->instance();

	rInside[".readDatasetToEndNative"] = Rcpp::InternalFunction(&rbridge_readDataSetSEXP);
	rInside[".readDataSetHeaderNative"] = Rcpp::InternalFunction(&rbridge_readDataSetHeaderSEXP);
	rInside[".callbackNative"] = Rcpp::InternalFunction(&rbridge_callbackSEXP);
	rInside[".requestTempFileNameNative"] = Rcpp::InternalFunction(&rbridge_requestTempFileNameSEXP);
	rInside[".requestStateFileNameNative"] = Rcpp::InternalFunction(&rbridge_requestStateFileNameSEXP);
	rInside[".baseCitation"] = "JASP Team (" + AppInfo::getBuildYear() + "). JASP (Version " + AppInfo::version.asString() + ") [Computer software].";

	rInside["jasp.analyses"] = Rcpp::List();
	rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"JASP\"))");
	rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"methods\"))");
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

SEXP rbridge_requestTempFileNameSEXP(SEXP extension)
{
	if (rbridge_fileNameSource == NULL)
		return R_NilValue;

	string extensionAsString = Rcpp::as<string>(extension);
	string root;
	string relativePath;

	rbridge_fileNameSource(extensionAsString, root, relativePath);

	Rcpp::List paths;

	paths["root"] = root;
	paths["relativePath"] = relativePath;

	return paths;
}

SEXP rbridge_requestStateFileNameSEXP()
{
	if (rbridge_stateFileSource == NULL)
		return R_NilValue;

	string root;
	string relativePath;

	rbridge_stateFileSource(root, relativePath);

	Rcpp::List paths;

	paths["root"] = root;
	paths["relativePath"] = relativePath;

	return paths;
}

string rbridge_run(const string &name, const string &options, const string &perform, int ppi, RCallback callback)
{
	SEXP results;

	rbridge_runCallback = callback;

	RInside &rInside = rbridge_rinside->instance();

	rInside["name"] = name;
	rInside["options.as.json.string"] = options;
	rInside["perform"] = perform;
	rInside[".ppi"] = ppi;

	rInside.parseEval("run(name=name, options.as.json.string=options.as.json.string, perform)", results);

	rbridge_runCallback = NULL;

	return Rcpp::as<string>(results);
}

Rcpp::DataFrame rbridge_readDataSet(const std::map<std::string, Column::ColumnType> &columns)
{
	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	Rcpp::List list(columns.size());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	typedef pair<const string, Column::ColumnType> ColumnInfo;

	BOOST_FOREACH(const ColumnInfo &columnInfo, columns)
	{
		(void)columns;

		string columnName = columnInfo.first;

		string base64 = Base64::encode("X", columnName, Base64::RVarEncoding);
		columnNames.push_back(base64);

		Column &column = rbridge_dataSet->columns().get(columnName);
		Column::ColumnType columnType = column.columnType();

		Column::ColumnType requestedType = columnInfo.second;
		if (requestedType == Column::ColumnTypeUnknown)
			requestedType = columnType;

		int rowCount = column.rowCount();
		int rowNo = 0;

		if (requestedType == Column::ColumnTypeScale)
		{
			if (columnType == Column::ColumnTypeScale)
			{
				Rcpp::NumericVector v(rowCount);

				BOOST_FOREACH(double value, column.AsDoubles)
				{
					(void)column;
					v[rowNo++] = value;
				}

				list[colNo++] = v;
			}
			else if (columnType == Column::ColumnTypeOrdinal || columnType == Column::ColumnTypeNominal)
			{
				Rcpp::IntegerVector v(rowCount);

				BOOST_FOREACH(int value, column.AsInts)
				{
					(void)column;
					v[rowNo++] = value;
				}

				list[colNo++] = v;
			}
			else // columnType == Column::ColumnTypeNominalText
			{
				Rcpp::IntegerVector v(rowCount);

				BOOST_FOREACH(int value, column.AsInts)
				{
					(void)column;
					if (value == INT_MIN)
						v[rowNo++] = INT_MIN;
					else
						v[rowNo++] = value + 1;
				}

				rbridge_makeFactor(v, column.labels());

				list[colNo++] = v;
			}
		}
		else // if (requestedType != Column::ColumnTypeScale)
		{
			bool ordinal = (requestedType == Column::ColumnTypeOrdinal);

			Rcpp::IntegerVector v(rowCount);

			if (columnType != Column::ColumnTypeScale)
			{
				std::map<int, int> indices;
				int i = 1; // R starts indices from 1

				const Labels &labels = column.labels();

				BOOST_FOREACH(const Label &label, labels)
				{
					indices[label.value()] = i++;
				}

				BOOST_FOREACH(int value, column.AsInts)
				{
					(void)column;
					if (value == INT_MIN)
						v[rowNo++] = INT_MIN;
					else
						v[rowNo++] = indices.at(value);
				}

				rbridge_makeFactor(v, column.labels(), ordinal);
			}
			else
			{
				// scale to nominal or ordinal (doesn't really make sense, but we have to do something)

				set<int> uniqueValues;

				BOOST_FOREACH(double value, column.AsDoubles)
				{
					(void)column;

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

				BOOST_FOREACH(int value, uniqueValues)
				{
					(void)value;
					(void)uniqueValues;

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

				BOOST_FOREACH(double value, column.AsDoubles)
				{
					(void)column;

					if (std::isnan(value))
						v[rowNo] = INT_MIN;
					else if (isfinite(value))
						v[rowNo] = valueToIndex[(int)(value * 1000)] + 1;
					else if (value > 0)
						v[rowNo] = valueToIndex[INT_MAX] + 1;
					else
						v[rowNo] = valueToIndex[INT_MIN] + 1;

					rowNo++;
				}

				rbridge_makeFactor(v, labels, ordinal);
			}

			list[colNo++] = v;
		}
	}

	list.attr("names") = columnNames;

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame(list);

	return dataFrame;
}

Rcpp::DataFrame rbridge_readDataSetHeader(const std::map<string, Column::ColumnType> &columns)
{
	if (rbridge_dataSet == NULL)
		rbridge_dataSet = rbridge_dataSetSource();

	Rcpp::List list(columns.size());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	typedef pair<const string, Column::ColumnType> ColumnInfo;

	BOOST_FOREACH(const ColumnInfo &columnInfo, columns)
	{
		(void)columns;

		string columnName = columnInfo.first;

		string base64 = Base64::encode("X", columnName, Base64::RVarEncoding);
		columnNames.push_back(base64);

		Columns &columns = rbridge_dataSet->columns();
		Column &column = columns.get(columnName);
		Column::ColumnType columnType = column.columnType();

		Column::ColumnType requestedType = columnInfo.second;
		if (requestedType == Column::ColumnTypeUnknown)
			requestedType = columnType;

		if (requestedType == Column::ColumnTypeScale)
		{
			if (columnType == Column::ColumnTypeScale)
			{
				list[colNo++] = Rcpp::NumericVector(0);
			}
			else if (columnType == Column::ColumnTypeOrdinal || columnType == Column::ColumnTypeNominal)
			{
				list[colNo++] = Rcpp::IntegerVector(0);
			}
			else
			{
				Rcpp::IntegerVector v(0);
				rbridge_makeFactor(v, column.labels());
				list[colNo++] = v;
			}
		}
		else
		{
			bool ordinal = (requestedType == Column::ColumnTypeOrdinal);

			Rcpp::IntegerVector v(0);
			rbridge_makeFactor(v, column.labels(), ordinal);

			list[colNo++] = v;
		}
	}

	list.attr("names") = columnNames;

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame(list);

	return dataFrame;
}

void rbridge_makeFactor(Rcpp::IntegerVector &v, const Labels &levels, bool ordinal)
{
	Rcpp::CharacterVector labels;

	if (levels.size() == 0)
	{
		labels.push_back(".");
	}
	else
	{
		BOOST_FOREACH(const Label &level, levels)
			labels.push_back(level.text());
	}

	v.attr("levels") = labels;

	vector<string> cla55;
	if (ordinal)
		cla55.push_back("ordered");
	cla55.push_back("factor");

	v.attr("class") = cla55;
}

void rbridge_makeFactor(Rcpp::IntegerVector &v, const std::vector<string> &levels, bool ordinal)
{
	v.attr("levels") = levels;
	vector<string> cla55;
	if (ordinal)
		cla55.push_back("ordered");
	cla55.push_back("factor");

	v.attr("class") = cla55;
}


SEXP rbridge_callback(SEXP results)
{
	if (rbridge_runCallback != NULL)
	{
		if (Rf_isNull(results))
		{
			return Rcpp::CharacterVector(rbridge_runCallback("null"));
		}
		else
		{
			return Rcpp::CharacterVector(rbridge_runCallback(Rcpp::as<string>(results)));
		}
	}
	else
	{
		return 0;
	}
}

std::map<string, Column::ColumnType> rbridge_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	map<string, Column::ColumnType> columnsRequested;

	if (Rf_isLogical(allColumns) && Rcpp::as<bool>(allColumns))
	{
		if (rbridge_dataSet == NULL)
			rbridge_dataSet = rbridge_dataSetSource();

		BOOST_FOREACH(const Column &column, rbridge_dataSet->columns())
			columnsRequested[column.name()] = Column::ColumnTypeUnknown;
	}

	if (Rf_isString(columns))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columns);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeUnknown;
	}

	if (Rf_isString(columnsAsNumeric))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsNumeric);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeScale;
	}

	if (Rf_isString(columnsAsOrdinal))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsOrdinal);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeOrdinal;
	}

	if (Rf_isString(columnsAsNominal))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsNominal);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeNominal;
	}

	return columnsRequested;
}

SEXP rbridge_callbackSEXP(SEXP results)
{
	return rbridge_callback(results);
}

Rcpp::DataFrame rbridge_readDataSetSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	map<string, Column::ColumnType> columnsRequested = rbridge_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns);
	return rbridge_readDataSet(columnsRequested);
}

Rcpp::DataFrame rbridge_readDataSetHeaderSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	map<string, Column::ColumnType> columnsRequested = rbridge_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns);
	return rbridge_readDataSetHeader(columnsRequested);
}

string rbridge_check()
{
	SEXP result = rbridge_rinside->parseEvalNT("checkPackages()");
	if (Rf_isString(result))
		return Rcpp::as<string>(result);
	else
		return "null";
}

string rbridge_saveImage(const string &name, const string &type, const int &height, const int &width, const int ppi)

{
	RInside &rInside = rbridge_rinside->instance();

	rInside["plotName"] = name;
	rInside["format"] = type;

	rInside["height"] = height;
	rInside["width"] = width;
	rInside[".ppi"] = ppi;

	SEXP result = rbridge_rinside->parseEvalNT("saveImage(plotName,format,height,width)");

	if (Rf_isString(result))
		return Rcpp::as<string>(result);
	else
		return "null";
}
