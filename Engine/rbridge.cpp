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
#include "jsonredirect.h"
#include "sharedmemory.h"
#include "appinfo.h"
#include "tempfiles.h"
#include "log.h"
#include "timers.h"
#include "r_functionwhitelist.h"
#include "otoolstuff.h"

DataSet						*	rbridge_dataSet		= nullptr;
RCallback						rbridge_callback	= NULL;
std::set<std::string>			filterColumnsUsed;
std::vector<std::string>		columnNamesInDataSet;
ColumnEncoder				*	extraEncodings		= nullptr;


//You cannot replace these NULL's by nullptr because then the compiler will complain about expressions that cannot be used as functions
boost::function<DataSet *()>				rbridge_dataSetSource		= NULL;
boost::function<size_t()>					rbridge_getDataSetRowCount	= NULL;
boost::function<int(const std::string &)>	rbridge_getColumnTypeEngine = NULL;

boost::function<void(const std::string &, std::string &, std::string &)>	rbridge_fileNameSource			= NULL,
																			rbridge_specificFileNameSource	= NULL;
boost::function<void(std::string &, std::string &)>							rbridge_stateFileSource			= NULL,
																			rbridge_jaspResultsFileSource	= NULL;

boost::function<bool(const std::string &, const	std::vector<double>&)											> rbridge_setColumnDataAsScaleEngine		= NULL;
boost::function<bool(const std::string &,		std::vector<int>&,			const std::map<int, std::string>&)	> rbridge_setColumnDataAsOrdinalEngine		= NULL;
boost::function<bool(const std::string &,		std::vector<int>&,			const std::map<int, std::string>&)	> rbridge_setColumnDataAsNominalEngine		= NULL;
boost::function<bool(const std::string &, const	std::vector<std::string>&)										> rbridge_setColumnDataAsNominalTextEngine	= NULL;

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

void rbridge_init(sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction, ColumnEncoder * extraEncoder, const char * resultsFont)
{
	JASPTIMER_SCOPE(rbridge_init);
	
	Log::log() << "Setting extraEncodings." << std::endl;
	extraEncodings = extraEncoder;

	Log::log() << "Collecting RBridgeCallBacks." << std::endl;
	RBridgeCallBacks callbacks = {
		rbridge_readDataSet,
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
		rbridge_setColumnAsScale,
		rbridge_setColumnAsOrdinal,
		rbridge_setColumnAsNominal,
		rbridge_setColumnAsNominalText,
		rbridge_dataSetRowCount,
		rbridge_encodeColumnName,
		rbridge_decodeColumnName,
		rbridge_encodeAllColumnNames,
		rbridge_decodeAllColumnNames,
		rbridge_allColumnNames
	};

	JASPTIMER_START(jaspRCPP_init);
	
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
					resultsFont,
					rbridge_nativeToUtf8
	);
	JASPTIMER_STOP(jaspRCPP_init);

}

void rbridge_junctionHelper(bool collectNotRestore, const std::string & folder)
{
	jaspRCPP_junctionHelper(collectNotRestore, folder.c_str());	
}

void rbridge_setDataSetSource(			boost::function<DataSet* ()> source)												{	rbridge_dataSetSource			= source; }
void rbridge_setFileNameSource(			boost::function<void (const std::string &, std::string &, std::string &)> source)	{	rbridge_fileNameSource			= source; }
void rbridge_setSpecificFileNameSource(	boost::function<void (const std::string &, std::string &, std::string &)> source)	{	rbridge_specificFileNameSource	= source; }
void rbridge_setStateFileSource(		boost::function<void (std::string &, std::string &)> source)						{	rbridge_stateFileSource			= source; }
void rbridge_setJaspResultsFileSource(	boost::function<void (std::string &, std::string &)> source)						{	rbridge_jaspResultsFileSource	= source; }

void rbridge_setColumnFunctionSources(			boost::function<int (const std::string &)																		> getTypeSource,
												boost::function<bool(const std::string &, const std::vector<double>		&)										> scaleSource,
												boost::function<bool(const std::string &,		std::vector<int>		&,	const std::map<int, std::string>&)	> ordinalSource,
												boost::function<bool(const std::string &,		std::vector<int>		&,	const std::map<int, std::string>&)	> nominalSource,
												boost::function<bool(const std::string &, const std::vector<std::string>&)										> nominalTextSource)
{
	rbridge_getColumnTypeEngine					= getTypeSource;
	rbridge_setColumnDataAsScaleEngine			= scaleSource;
	rbridge_setColumnDataAsOrdinalEngine		= ordinalSource;
	rbridge_setColumnDataAsNominalEngine		= nominalSource;
	rbridge_setColumnDataAsNominalTextEngine	= nominalTextSource;
}

void rbridge_setGetDataSetRowCountSource(boost::function<int()> source)	{	rbridge_getDataSetRowCount = source;	}

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

extern "C" const char * STDCALL rbridge_nativeToUtf8(const char * in)
{
	static std::string out;
#ifdef _WIN32
	//I cannot use Qt here, because even though "Qt += core" it seems that whenever I include QString here jaspEngine just dies immediately. And I *only* wanted to use fromLocal8Bit.. 
	//But I can't so let me be very inspired by https://stackoverflow.com/a/21575607 who came up with the following:
	
	std::string codepage_str(in);
	
	int size = MultiByteToWideChar(CP_ACP, MB_COMPOSITE, codepage_str.c_str(), codepage_str.length(), nullptr, 0);
	std::wstring utf16_str(size, '\0');
	
	MultiByteToWideChar(CP_ACP, MB_COMPOSITE, codepage_str.c_str(), codepage_str.length(), &utf16_str[0], size);
	
	int utf8_size = WideCharToMultiByte(CP_UTF8, 0, utf16_str.c_str(), utf16_str.length(), nullptr, 0, nullptr, nullptr);
	std::string utf8_str(utf8_size, '\0');
	
	WideCharToMultiByte(CP_UTF8, 0, utf16_str.c_str(), utf16_str.length(), &utf8_str[0], utf8_size,	nullptr, nullptr);
	
	out = utf8_str;
#else
	out = in;
#endif
	return out.c_str();
}



extern "C" bool STDCALL rbridge_requestJaspResultsFileSource(const char** root, const char **relativePath)
{
	if (!rbridge_stateFileSource)
		return false;

	static std::string _root;
	static std::string _relativePath;

	rbridge_jaspResultsFileSource(_root, _relativePath);

	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}


extern "C" bool STDCALL rbridge_requestStateFileSource(const char** root, const char **relativePath)
{
	if (!rbridge_stateFileSource)
		return false;

	static std::string _root;
	static std::string _relativePath;

	rbridge_stateFileSource(_root, _relativePath);

	*root = _root.c_str();
	*relativePath = _relativePath.c_str();
	return true;
}

extern "C" bool STDCALL rbridge_requestTempFileName(const char* extensionAsString, const char** root, const char** relativePath)
{
	if (!rbridge_fileNameSource)
		return false;

	static std::string _root, _relativePath;

	rbridge_fileNameSource(extensionAsString, _root, _relativePath);
	*root			= _root.c_str();
	*relativePath	= _relativePath.c_str();
	return true;
}

extern "C" bool STDCALL rbridge_requestSpecificFileName(const char* specificFilename, const char** root, const char** relativePath)
{
	if (!rbridge_specificFileNameSource)
		return false;

	static std::string _root, _relativePath, _specific;

	_specific = specificFilename;

	rbridge_specificFileNameSource(_specific, _root, _relativePath);

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

std::string rbridge_runModuleCall(const std::string &name, const std::string &title, const std::string &moduleCall, const std::string &dataKey, const std::string &options, const std::string &stateKey, int ppi, int analysisID, int analysisRevision, const std::string &imageBackground, bool developerMode, const std::string &resultsFont)
{
	rbridge_callback	= NULL; //Only jaspResults here so callback is not needed
	if (rbridge_dataSet != nullptr)
		rbridge_dataSet		= rbridge_dataSetSource();

	return jaspRCPP_runModuleCall(name.c_str(), title.c_str(), moduleCall.c_str(), dataKey.c_str(), options.c_str(), stateKey.c_str(), ppi, analysisID, analysisRevision, imageBackground.c_str(), developerMode, resultsFont.c_str());
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
	rbridge_dataSet = rbridge_dataSetSource();

	if(rbridge_dataSet == nullptr)
		return nullptr;

	Columns &columns = rbridge_dataSet->columns();

	(*colMax) = columns.columnCount();
	RBridgeColumnType* colHeaders = (RBridgeColumnType*)calloc((*colMax), sizeof(RBridgeColumnType));

	for(int i=0; i<(*colMax); i++)
	{
		colHeaders[i].name = strdup(ColumnEncoder::columnEncoder()->encode(columns[i].name()).c_str());
		colHeaders[i].type = (int)columns[i].getColumnType();
	}

	RBridgeColumn * returnThis = rbridge_readDataSet(colHeaders, (*colMax), obeyFilter);

	for(int i=0; i<(*colMax); i++)
		free(colHeaders[i].name);

	free(colHeaders);

	return returnThis;
}

extern "C" RBridgeColumn* STDCALL rbridge_readDataSetForFiltering(size_t * colMax)
{
	rbridge_dataSet = rbridge_dataSetSource();

	Columns &columns = rbridge_dataSet->columns();

	(*colMax) = filterColumnsUsed.size();

	if(*colMax == 0)
		return nullptr;

	RBridgeColumnType* colHeaders = (RBridgeColumnType*)calloc((*colMax), sizeof(RBridgeColumnType));

	for(size_t iIn=0, iOut=0; iIn < columns.columnCount() && iOut < filterColumnsUsed.size(); iIn++)
		if(filterColumnsUsed.count(columns[iIn].name()) > 0)
		{
			colHeaders[iOut].name = strdup(ColumnEncoder::columnEncoder()->encode(columns[iIn].name()).c_str());
			colHeaders[iOut].type = (int)columns[iIn].getColumnType();

			iOut++;
		}

	RBridgeColumn * returnThis = rbridge_readDataSet(colHeaders, (*colMax), false);

	for(int i=0; i<(*colMax); i++)
		free(colHeaders[i].name);
	free(colHeaders);

	return returnThis;
}

static RBridgeColumn*	datasetStatic = nullptr;
static int				datasetColMax = 0;

extern "C" RBridgeColumn* STDCALL rbridge_readDataSet(RBridgeColumnType* colHeaders, size_t colMax, bool obeyFilter)
{
	if (colHeaders == nullptr)
		return nullptr;

	rbridge_dataSet = rbridge_dataSetSource();

	if(rbridge_dataSet == nullptr)
		return nullptr;

	Columns &columns = rbridge_dataSet->columns();

	if (datasetStatic != nullptr)
		freeRBridgeColumns();

	datasetColMax = colMax;
	datasetStatic = static_cast<RBridgeColumn*>(calloc(datasetColMax + 1, sizeof(RBridgeColumn)));

	size_t filteredRowCount = obeyFilter ? rbridge_dataSet->filteredRowCount() : rbridge_dataSet->rowCount();

	// lets make some rownumbers/names for R that takes into account being filtered or not!
	datasetStatic[colMax].ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));
	datasetStatic[colMax].nbRows	= filteredRowCount;
	int filteredRow					= 0;

	//If you change anything here, make sure that "label outliers" in Descriptives still works properly (including with filters)
	for(size_t i=0; i<rbridge_dataSet->rowCount() && filteredRow < datasetStatic[colMax].nbRows; i++)
		if(!obeyFilter || (rbridge_dataSet->filterVector().size() > i && rbridge_dataSet->filterVector()[i]))
			datasetStatic[colMax].ints[filteredRow++] = int(i + 1); //R needs 1-based index

	//std::cout << "reading " << colMax << " columns!\nRowCount: " << filteredRowCount << "" << std::endl;

	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType	&	columnInfo		= colHeaders[colNo];
		RBridgeColumn		&	resultCol		= datasetStatic[colNo];
		std::string				columnName		= ColumnEncoder::columnEncoder()->decode(columnInfo.name);
								resultCol.name	= strdup(columnInfo.name);
		Column				&	column			= columns.get(columnName);
		columnType				colType			= column.getColumnType(),
								requestedType	= columnType(columnInfo.type);

		if (requestedType == columnType::unknown)
			requestedType = colType;

		//int rowCount = column.rowCount();
		resultCol.nbRows = filteredRowCount;
		int rowNo = 0, dataSetRowNo = 0;

		if (requestedType == columnType::scale)
		{
			if (colType == columnType::scale)
			{
				resultCol.isScale	= true;
				resultCol.hasLabels	= false;
				resultCol.doubles	= (double*)calloc(filteredRowCount, sizeof(double));

				for(double value : column.AsDoubles)
					if(rowNo < filteredRowCount && (!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++]))
						resultCol.doubles[rowNo++] = value;
			}
			else if (colType == columnType::ordinal || colType == columnType::nominal)
			{
				resultCol.isScale	= false;
				resultCol.hasLabels	= false;
				resultCol.ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));

				for(int value : column.AsInts)
					if(rowNo < filteredRowCount && (!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++]))
						resultCol.ints[rowNo++] = value;
			}
			else // columnType == ColumnType::nominalText
			{
				resultCol.isScale	= false;
				resultCol.hasLabels = true;
				resultCol.isOrdinal = false;
				resultCol.ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));

				for(int value : column.AsInts)
					if(rowNo < filteredRowCount && (!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++]))
					{
						if (value == INT_MIN)	resultCol.ints[rowNo++] = INT_MIN;
						else					resultCol.ints[rowNo++] = value;
					}

				resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
			}
		}
		else // if (requestedType != ColumnType::scale)
		{
			resultCol.isScale	= false;
			resultCol.hasLabels	= true;
			resultCol.ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));
			resultCol.isOrdinal = (requestedType == columnType::ordinal);

			if (colType != columnType::scale)
			{
				std::map<int, int> indices;
				int i = 1; // R starts indices from 1

				const Labels &labels = column.labels();

				for(const Label &label : labels)
					indices[label.value()] = i++;

				for(int value : column.AsInts)
					if(rowNo < filteredRowCount && (!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++]))
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
				resultCol.ints		= filteredRowCount == 0 ? nullptr : static_cast<int*>(calloc(filteredRowCount, sizeof(int)));

				std::set<int> uniqueValues;

				for(double value : column.AsDoubles)
				{

					if (std::isnan(value))
						continue;

					int intValue;

					if (std::isfinite(value))	intValue = (int)(value * 1000);
					else if (value < 0)			intValue = INT_MIN;
					else						intValue = INT_MAX;

					uniqueValues.insert(intValue);
				}

				int index = 0;
				std::map<int, int> valueToIndex;
				std::vector<std::string> labels;

				for(int value : uniqueValues)
				{
					valueToIndex[value] = index++;

					if (value == INT_MAX)		labels.push_back("Inf");
					else if (value == INT_MIN)	labels.push_back("-Inf");
					else
					{
						std::stringstream ss;
						ss << ((double)value / 1000);
						labels.push_back(ss.str());
					}
				}

				for(double value : column.AsDoubles)
					if(rowNo < filteredRowCount && (!obeyFilter || rbridge_dataSet->filterVector()[dataSetRowNo++]))
					{

						if (std::isnan(value))			resultCol.ints[rowNo] = INT_MIN;
						else if (std::isfinite(value))	resultCol.ints[rowNo] = valueToIndex[(int)(value * 1000)] + 1;
						else if (value > 0)				resultCol.ints[rowNo] = valueToIndex[INT_MAX] + 1;
						else							resultCol.ints[rowNo] = valueToIndex[INT_MIN] + 1;

						rowNo++;
					}

				resultCol.labels = rbridge_getLabels(labels, resultCol.nbLabels);
			}
		}
	}

	return datasetStatic;
}

extern "C" char** STDCALL rbridge_readDataColumnNames(size_t * colMax)
{
					rbridge_dataSet = rbridge_dataSetSource();

	if(!rbridge_dataSet)
	{
		*colMax = 0;
		return nullptr;
	}

	Columns		&	columns			= rbridge_dataSet->columns();
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
	for (const Column &column: columns)
		staticResult[colNo++] = strdup(ColumnEncoder::columnEncoder()->encode(column.name()).c_str());

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

	lastColMax			= colMax;
	resultCols			= static_cast<RBridgeColumnDescription*>(calloc(colMax, sizeof(RBridgeColumnDescription)));
	rbridge_dataSet		= rbridge_dataSetSource();
	Columns &columns	= rbridge_dataSet->columns();

	for (int colNo = 0; colNo < colMax; colNo++)
	{
		RBridgeColumnType			&	columnInfo		= columnsType[colNo];
		RBridgeColumnDescription	&	resultCol		= resultCols[colNo];
		std::string						columnName		= ColumnEncoder::columnEncoder()->decode(columnInfo.name);
										resultCol.name	= strdup(columnInfo.name);
		Column						&	column			= columns.get(columnName);
		columnType						colType			= column.getColumnType(),
										requestedType	= columnType(columnInfo.type);

		if (requestedType == columnType::unknown)
			requestedType = colType;

		if (requestedType == columnType::scale)
		{
			resultCol.isScale	= colType == columnType::scale;
			resultCol.hasLabels = colType == columnType::nominalText;
			resultCol.isOrdinal = colType == columnType::ordinal; //Should I do this? Originally it was only set to false when nominaltext and not set at all in other cases...

			if(colType == columnType::nominalText)
				resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
		}
		else
		{
			resultCol.isScale	= false;
			resultCol.hasLabels = true;
			resultCol.isOrdinal = (requestedType == columnType::ordinal);

			if (colType != columnType::scale)
				resultCol.labels = rbridge_getLabels(column.labels(), resultCol.nbLabels);
			else
			{
				// scale to nominal or ordinal (doesn't really make sense, but we have to do something)
				std::set<int> uniqueValues;

				for (double value: column.AsDoubles)
				{
					if (std::isnan(value))
						continue;

					int intValue;

					if (std::isfinite(value))	intValue = (int)(value * 1000);
					else if (value < 0)			intValue = INT_MIN;
					else						intValue = INT_MAX;

					uniqueValues.insert(intValue);
				}

				std::vector<std::string> labels;

				for (int value: uniqueValues)
				{
					if (value == INT_MAX)			labels.push_back("Inf");
					else if (value == INT_MIN)		labels.push_back("-Inf");
					else							labels.push_back(std::to_string((double)value / 1000.0f));
				}

				resultCol.labels = rbridge_getLabels(labels, resultCol.nbLabels);

			}

		}
	}

	return resultCols;
}

#define JASP_COLUMN_DECODE_HERE std::string colName(ColumnEncoder::columnEncoder()->decode(columnName))

extern "C" int STDCALL rbridge_getColumnType(const char * columnName)
{
	JASP_COLUMN_DECODE_HERE;
	return rbridge_getColumnTypeEngine(colName);
}

extern "C" bool STDCALL rbridge_setColumnAsScale(const char* columnName, double * scalarData, size_t length)
{
	JASP_COLUMN_DECODE_HERE;

	std::vector<double> scalars(scalarData, scalarData + length);

	return rbridge_setColumnDataAsScaleEngine(colName, scalars);
}

extern "C" bool STDCALL rbridge_setColumnAsOrdinal(const char* columnName, int * ordinalData, size_t length, const char ** levels, size_t numLevels)
{
	JASP_COLUMN_DECODE_HERE;

	std::vector<int> ordinals(ordinalData, ordinalData + length);

	std::map<int, std::string> labels;
	for(size_t lvl=0; lvl<numLevels; lvl++)
		labels[lvl + 1] = levels[lvl];

	return rbridge_setColumnDataAsOrdinalEngine(colName, ordinals, labels);
}

extern "C" bool STDCALL rbridge_setColumnAsNominal(const char* columnName, int * nominalData, size_t length, const char ** levels, size_t numLevels)
{
	JASP_COLUMN_DECODE_HERE;

	std::vector<int> nominals(nominalData, nominalData + length);

	std::map<int, std::string> labels;
	for(size_t lvl=0; lvl<numLevels; lvl++)
		labels[lvl + 1] = levels[lvl];

	return rbridge_setColumnDataAsNominalEngine(colName, nominals, labels);
}

extern "C" bool STDCALL rbridge_setColumnAsNominalText(const char* columnName, const char ** nominalData, size_t length)
{
	JASP_COLUMN_DECODE_HERE;

	std::vector<std::string> nominals(nominalData, nominalData + length);

	return rbridge_setColumnDataAsNominalTextEngine(colName, nominals);
}

extern "C" int	STDCALL rbridge_dataSetRowCount()
{
	return rbridge_getDataSetRowCount();
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

		if (column.hasLabels)
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
		if (column.hasLabels)
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

char** rbridge_getLabels(const Labels &levels, size_t &nbLevels)
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
		for (const Label &level: levels)
			results[i++] = strdup(level.text().c_str());
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
	rbridge_dataSet = rbridge_dataSetSource();

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
		throw filterException("Filtered out all data..");

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
	rbridge_dataSet = rbridge_dataSetSource();
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

