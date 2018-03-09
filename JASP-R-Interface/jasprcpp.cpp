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

#include "jasprcpp.h"
#include "rinside_consolelogging.h"

using namespace std;

RInside *rinside;
RInside_ConsoleLogging *rinside_consoleLog;
ReadDataSetCB readDataSetCB;
RunCallbackCB runCallbackCB;
ReadADataSetCB readFullDataSetCB;
ReadADataSetCB readFilterDataSetCB;
ReadDataColumnNamesCB readDataColumnNamesCB;
RequestTempFileNameCB requestTempFileNameCB;
RequestTempRootNameCB requestTempRootNameCB;
ReadDataSetDescriptionCB readDataSetDescriptionCB;
RequestStateFileSourceCB requestStateFileSourceCB;

static const string NullString = "null";
static std::string lastFilterErrorMsg = "";


extern "C" {
void STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks* callbacks)
{
	rinside = new RInside();
	rinside_consoleLog = new RInside_ConsoleLogging();
	rinside->set_callbacks(rinside_consoleLog);

	RInside &rInside = rinside->instance();

	runCallbackCB							= callbacks->runCallbackCB;
	readDataSetCB							= callbacks->readDataSetCB;
	readFullDataSetCB						= callbacks->readFullDataSetCB;
	readFilterDataSetCB						= callbacks->readFilterDataSetCB;
	readDataColumnNamesCB					= callbacks->readDataColumnNamesCB;
	requestTempFileNameCB					= callbacks->requestTempFileNameCB;
	requestTempRootNameCB					= callbacks->requestTempRootNameCB;
	readDataSetDescriptionCB				= callbacks->readDataSetDescriptionCB;
	requestStateFileSourceCB				= callbacks->requestStateFileSourceCB;

	rInside[".callbackNative"]				= Rcpp::InternalFunction(&jaspRCPP_callbackSEXP);
	rInside[".readDatasetToEndNative"]		= Rcpp::InternalFunction(&jaspRCPP_readDataSetSEXP);
	rInside[".readFullDatasetToEnd"]		= Rcpp::InternalFunction(&jaspRCPP_readFullDataSet);
	rInside[".returnDataFrame"]				= Rcpp::InternalFunction(&jaspRCPP_returnDataFrame);
	rInside[".returnString"]				= Rcpp::InternalFunction(&jaspRCPP_returnString);
	rInside[".readFilterDatasetToEnd"]		= Rcpp::InternalFunction(&jaspRCPP_readFilterDataSet);
	rInside[".readDataSetHeaderNative"]		= Rcpp::InternalFunction(&jaspRCPP_readDataSetHeaderSEXP);
	rInside[".requestTempFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestTempFileNameSEXP);
	rInside[".requestTempRootNameNative"] = Rcpp::InternalFunction(&jaspRCPP_requestTempRootNameSEXP);
	rInside[".requestStateFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestStateFileNameSEXP);

	static const char *baseCitationFormat = "JASP Team (%s). JASP (Version %s) [Computer software].";
	char baseCitation[200];
	sprintf(baseCitation, baseCitationFormat, buildYear, version);
	rInside[".baseCitation"] = baseCitation;

	rInside["jasp.analyses"] = Rcpp::List();
	rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"JASP\"))");
	rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"methods\"))");
	
	rinside->parseEvalNT("initEnvironment()");
}


const char* STDCALL jaspRCPP_run(const char* name, const char* title, bool requiresInit, const char* dataKey, const char* options, const char* resultsMeta, const char* stateKey, const char* perform, int ppi)
{
	SEXP results;

	RInside &rInside = rinside->instance();

	rInside["name"]			= name;
	rInside["title"]		= title;
	rInside["requiresInit"]	= requiresInit;
	rInside["dataKey"]		= dataKey;
	rInside["options"]		= options;
	rInside["resultsMeta"]	= resultsMeta;
	rInside["stateKey"]		= stateKey;
	rInside["perform"]		= perform;
	rInside[".ppi"]			= ppi;

	rInside.parseEval("run(name=name, title=title, requiresInit=requiresInit, dataKey=dataKey, options=options, resultsMeta=resultsMeta, stateKey=stateKey, perform=perform)", results);



	static string str;
	str = Rcpp::as<string>(results);
	return str.c_str();
}

const char* STDCALL jaspRCPP_check()
{
	SEXP result = rinside->parseEvalNT("checkPackages()");
	static string staticResult;

	staticResult = Rf_isString(result) ? Rcpp::as<string>(result) : NullString;
	return staticResult.c_str();
}

void STDCALL jaspRCPP_runScript(const char * scriptCode)
{
	SEXP result = rinside->parseEvalNT(scriptCode);

	return;
}



int STDCALL jaspRCPP_runFilter(const char * filterCode, bool ** arrayPointer)
{
	rinside_consoleLog->clearConsoleBuffer();

	try
	{
		SEXP result = rinside->parseEval(filterCode);

		if(Rf_isVector(result))
		{
			Rcpp::NumericVector vec(result);

			if(vec.size() == 0)
				return 0;

			(*arrayPointer) = (bool*)malloc(vec.size() * sizeof(bool));

			for(int i=0; i<vec.size(); i++)
				(*arrayPointer)[i] = vec[i] == 1;

			return vec.size();
		}

		lastFilterErrorMsg =  "Filter did not return a logical vector";
		return -1;
	}
	catch(std::exception e)
	{
#ifdef __WIN32__
		lastFilterErrorMsg = "Your filter has a problem, it might be a misspelled column-name or a syntax-error, but sadly enough JASP cannot capture R's output on Windows to give a clearer message..";
#else
		lastFilterErrorMsg =  std::string(e.what()) + "\n" + rinside_consoleLog->getConsoleOutput();
#endif
		return -1;
	}

	lastFilterErrorMsg = "something went wrong with the filter but it is unclear what";

	return -1;
}

const char*	STDCALL jaspRCPP_getLastFilterErrorMsg()
{
	return lastFilterErrorMsg.c_str();
}

void STDCALL jaspRCPP_freeArrayPointer(bool ** arrayPointer)
{
    free(*arrayPointer);
}

const char* STDCALL jaspRCPP_saveImage(const char *name, const char *type, const int height, const int width, const int ppi)
{
	RInside &rInside = rinside->instance();

	rInside["plotName"]	= name;
	rInside["format"]	= type;

	rInside["height"]	= height;
	rInside["width"]	= width;
	rInside[".ppi"]		= ppi;

	SEXP result = rinside->parseEvalNT("saveImage(plotName,format,height,width)");
	static string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<string>(result) : NullString;
	return staticResult.c_str();
}

const char* STDCALL jaspRCPP_editImage(const char *name, const char *type, const int height, const int width, const int ppi) {
	
	RInside &rInside = rinside->instance();

	rInside["plotName"] = name;
	rInside["type"] = type;
	rInside["height"] = height;
	rInside["width"] = width;
	rInside[".ppi"] = ppi;

	SEXP result = rinside->parseEvalNT("editImage(plotName,type,height,width)");
	static string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<string>(result) : NullString;

	return staticResult.c_str();

}

const char*	STDCALL jaspRCPP_evalRCode(const char *rCode) {
	// Function to evaluate arbitrary R code from C++
	// Returns string if R result is a string, else returns "null"
	SEXP result = rinside->parseEvalNT(rCode);
	static string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<string>(result) : NullString;
	return staticResult.c_str();
}

} // extern "C"

const char* STDCALL jaspRCPP_getRConsoleOutput()
{
	static std::string output;
	output = rinside_consoleLog->getConsoleOutput();
	return output.c_str();
}

SEXP jaspRCPP_requestTempFileNameSEXP(SEXP extension)
{
	const char *root, *relativePath;
	string extensionAsString = Rcpp::as<string>(extension);

	if (!requestTempFileNameCB(extensionAsString.c_str(), &root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"] = root;
	paths["relativePath"] = relativePath;

	return paths;
}

SEXP jaspRCPP_requestTempRootNameSEXP()
{
	const char* root = requestTempRootNameCB();

	Rcpp::List paths;
	paths["root"] = root;
	return paths;
}


SEXP jaspRCPP_requestStateFileNameSEXP()
{
	const char* root;
	const char* relativePath;

	if (!requestStateFileSourceCB(&root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"] = root;
	paths["relativePath"] = relativePath;

	return paths;
}


SEXP jaspRCPP_callbackSEXP(SEXP in, SEXP progress)
{
	string inStr = Rf_isNull(in) ? "null" : Rcpp::as<string>(in);
	int progressInt = Rf_isNull(progress) ? -1 : Rcpp::as<int>(progress);
	const char *out;
	bool ok = runCallbackCB(inStr.c_str(), progressInt, &out);
	if (ok)
	{
		return Rcpp::CharacterVector(out);
	}
	else
	{
		return 0;
	}
}

void jaspRCPP_returnDataFrame(Rcpp::DataFrame frame)
{
	int colcount = frame.size();

	std::cout << "got a dataframe!\n" << colcount << "X" << (colcount > 0 ? ((Rcpp::NumericVector)frame[0]).size() : -1) << "\n" << std::flush;

	if(colcount > 0)
	{
		int rowcount = ((Rcpp::NumericVector)frame[0]).size();

		for(int row=0; row<rowcount; row++)
		{
			for(int col=0; col<colcount; col++)
				std::cout << "'" << ((Rcpp::StringVector)frame[col])[row] << " or " <<  ((Rcpp::NumericVector)frame[col])[row]  << "'\t" << std::flush;

			std::cout << "\n";
		}
		std::cout << std::flush;
	}
}

void jaspRCPP_returnString(SEXP Message)
{
	std::cout << "A message from R: " << (std::string)((Rcpp::String)Message) << "\n" << std::flush;
}

RBridgeColumnType* jaspRCPP_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns, int *colMax)
{
	map<string, ColumnType> columnsRequested;

	if (Rf_isLogical(allColumns) && Rcpp::as<bool>(allColumns))
	{
		char** columns = readDataColumnNamesCB(colMax);
		if (columns)
		{
			for (int i = 0; i < *colMax; i++)
				columnsRequested[columns[i]] = ColumnTypeUnknown;
		}
	}

	if (Rf_isString(columns))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columns);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = ColumnTypeUnknown;
	}

	if (Rf_isString(columnsAsNumeric))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsNumeric);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = ColumnTypeScale;
	}

	if (Rf_isString(columnsAsOrdinal))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsOrdinal);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = ColumnTypeOrdinal;
	}

	if (Rf_isString(columnsAsNominal))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsNominal);
		for (size_t i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = ColumnTypeNominal;
	}

	RBridgeColumnType* result = (RBridgeColumnType*)calloc(columnsRequested.size(), sizeof(RBridgeColumnType));
	int colNo = 0;
	for (auto const &columnRequested : columnsRequested)
	{
		result[colNo].name = strdup(columnRequested.first.c_str());
		result[colNo].type = columnRequested.second;
		colNo++;
	}
	*colMax = colNo;

	return result;
}

void freeRBridgeColumnType(RBridgeColumnType *columns, int colMax)
{
	for (int i = 0; i < colMax; i++)
		free(columns[i].name);

	free(columns);
}

Rcpp::DataFrame jaspRCPP_readFullDataSet()
{
	int colMax = 0;
	RBridgeColumn* colResults = readFullDataSetCB(&colMax);
	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_readFilterDataSet()
{
	int colMax = 0;
	RBridgeColumn* colResults = readFilterDataSetCB(&colMax);
	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_readDataSetSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	int colMax = 0;
	RBridgeColumnType* columnsRequested = jaspRCPP_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns, &colMax);
	RBridgeColumn* colResults = readDataSetCB(columnsRequested, colMax, true);
	freeRBridgeColumnType(columnsRequested, colMax);

	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_convertRBridgeColumns_to_DataFrame(RBridgeColumn* colResults, int colMax)
{
	Rcpp::DataFrame dataFrame = Rcpp::DataFrame();

	if (colResults)
	{
		Rcpp::List list(colMax);
		Rcpp::CharacterVector columnNames;
		for (int i = 0; i < colMax; i++)
		{
			RBridgeColumn& colResult = colResults[i];
			columnNames.push_back(colResult.name);
			int maxRow = colResult.nbRows;
			if (colResult.isScale)
			{
				Rcpp::NumericVector v(maxRow);
				for (int j = 0; j < maxRow; j++)
					v[j] = colResult.doubles[j];

				list[i] = v;
			}
			else if (!colResult.hasLabels)
			{
				Rcpp::IntegerVector v(maxRow);
				for (int j = 0; j < maxRow; j++)
					v[j] = colResult.ints[j];
				list[i] = v;
			}
			else
			{
				Rcpp::IntegerVector v(maxRow);
				for (int j = 0; j < maxRow; j++)
					v[j] = colResult.ints[j];
				jaspRCPP_makeFactor(v, colResult.labels, colResult.nbLabels, colResult.isOrdinal);
				list[i] = v;
			}
		}
		list.attr("names") = columnNames;
		dataFrame = Rcpp::DataFrame(list);
	}

	return dataFrame;
}

Rcpp::DataFrame jaspRCPP_readDataSetHeaderSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	int colMax = 0;
	RBridgeColumnType* columnsRequested = jaspRCPP_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns, &colMax);
	RBridgeColumnDescription* columnsDescription = readDataSetDescriptionCB(columnsRequested, colMax);
	freeRBridgeColumnType(columnsRequested, colMax);

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame();

	if (columnsDescription)
	{
		Rcpp::List list(colMax);
		Rcpp::CharacterVector columnNames;

		for (int i = 0; i < colMax; i++)
		{
			RBridgeColumnDescription& colDescription = columnsDescription[i];
			columnNames.push_back(colDescription.name);
			if (colDescription.isScale)
			{
				list(i) = Rcpp::NumericVector(0);
			}
			else if (!colDescription.hasLabels)
			{
				list(i) = Rcpp::IntegerVector(0);
			}
			else
			{
				Rcpp::IntegerVector v(0);
				jaspRCPP_makeFactor(v, colDescription.labels, colDescription.nbLabels, colDescription.isOrdinal);
				list(i) = v;
			}
		}

		list.attr("names") = columnNames;
		dataFrame = Rcpp::DataFrame(list);
	}

	return dataFrame;

}

void jaspRCPP_makeFactor(Rcpp::IntegerVector &v, char** levels, int nbLevels, bool ordinal)
{
	Rcpp::CharacterVector labels;
	for (int i = 0; i < nbLevels; i++)
		labels.push_back(levels[i]);

	v.attr("levels") = labels;
	vector<string> cla55;
	if (ordinal)
		cla55.push_back("ordered");
	cla55.push_back("factor");

	v.attr("class") = cla55;
}
