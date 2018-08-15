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
#include "jaspResults/src/jaspResults.h"

#ifndef __WIN32__
RInside_ConsoleLogging *rinside_consoleLog;
#endif

RInside						*rinside;
ReadDataSetCB				readDataSetCB;
RunCallbackCB				runCallbackCB;
ReadADataSetCB				readFullDataSetCB,
							readFilterDataSetCB;
ReadDataColumnNamesCB		readDataColumnNamesCB;
RequestTempFileNameCB		requestTempFileNameCB;
RequestTempRootNameCB		requestTempRootNameCB;
ReadDataSetDescriptionCB	readDataSetDescriptionCB;
RequestSpecificFileSourceCB requestStateFileSourceCB,
							requestJaspResultsFileSourceCB;
static const	std::string NullString = "null";
static			std::string lastErrorMessage = "";
static			cetype_t Encoding = CE_UTF8;

SetColumnAsScale		dataSetColumnAsScale;
SetColumnAsOrdinal		dataSetColumnAsOrdinal;
SetColumnAsNominal		dataSetColumnAsNominal;
SetColumnAsNominalText	dataSetColumnAsNominalText;


extern "C" {
void jaspRCPP_send(const char * msg)
{
	printf("void jaspRCPP_send(const char * msg) got called!\nWith: \"%s\"\n", msg);
}

void STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks* callbacks, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction)
{
	rinside = new RInside();
#ifndef __WIN32__
	rinside_consoleLog = new RInside_ConsoleLogging();
	rinside->set_callbacks(rinside_consoleLog);
#endif

	RInside &rInside = rinside->instance();

	runCallbackCB							= callbacks->runCallbackCB;
	readDataSetCB							= callbacks->readDataSetCB;
	readFullDataSetCB						= callbacks->readFullDataSetCB;
	readFilterDataSetCB						= callbacks->readFilterDataSetCB;
	dataSetColumnAsScale					= callbacks->dataSetColumnAsScale;
	readDataColumnNamesCB					= callbacks->readDataColumnNamesCB;
	requestTempFileNameCB					= callbacks->requestTempFileNameCB;
	requestTempRootNameCB					= callbacks->requestTempRootNameCB;
	dataSetColumnAsOrdinal					= callbacks->dataSetColumnAsOrdinal;
	dataSetColumnAsNominal					= callbacks->dataSetColumnAsNominal;
	readDataSetDescriptionCB				= callbacks->readDataSetDescriptionCB;
	requestStateFileSourceCB				= callbacks->requestStateFileSourceCB;
	dataSetColumnAsNominalText				= callbacks->dataSetColumnAsNominalText;
	requestJaspResultsFileSourceCB			= callbacks->requestJaspResultsFileSourceCB;

	rInside[".setRError"]					= Rcpp::InternalFunction(&jaspRCPP_setRError);
	rInside[".setRWarning"]					= Rcpp::InternalFunction(&jaspRCPP_setRWarning);
	rInside[".returnString"]				= Rcpp::InternalFunction(&jaspRCPP_returnString);
	rInside[".callbackNative"]				= Rcpp::InternalFunction(&jaspRCPP_callbackSEXP);
	rInside[".returnDataFrame"]				= Rcpp::InternalFunction(&jaspRCPP_returnDataFrame);
	rInside[".setColumnDataAsScale"]		= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsScale);
	rInside[".readFullDatasetToEnd"]		= Rcpp::InternalFunction(&jaspRCPP_readFullDataSet);
	rInside[".setColumnDataAsOrdinal"]		= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsOrdinal);
	rInside[".setColumnDataAsNominal"]		= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsNominal);
	rInside[".readDatasetToEndNative"]		= Rcpp::InternalFunction(&jaspRCPP_readDataSetSEXP);
	rInside[".readFilterDatasetToEnd"]		= Rcpp::InternalFunction(&jaspRCPP_readFilterDataSet);
	rInside[".readDataSetHeaderNative"]		= Rcpp::InternalFunction(&jaspRCPP_readDataSetHeaderSEXP);
	rInside[".requestTempFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestTempFileNameSEXP);
	rInside[".requestTempRootNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestTempRootNameSEXP);
	rInside[".setColumnDataAsNominalText"]	= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsNominalText);
	rInside[".requestStateFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestStateFileNameSEXP);

	static const char *baseCitationFormat	= "JASP Team (%s). JASP (Version %s) [Computer software].";
	char baseCitation[200];
	sprintf(baseCitation, baseCitationFormat, buildYear, version);
	rInside[".baseCitation"] = baseCitation;

	jaspResults::setSendFunc(sendToDesktopFunction);
	jaspResults::setPollMessagesFunc(pollMessagesFunction);
	jaspTable::setCitation(baseCitation);
	rInside["jaspResultsModule"]			= givejaspResultsModule();

	//Adding some functions in R to the RefClass (generator) in the module
	rInside.parseEvalQ("jaspResultsModule$jaspTable$methods(addColumnInfo = function(name=NULL, title=NULL, overtitle=NULL, type=NULL, format=NULL, combine=NULL) { addColumnInfoHelper(name, title, type, format, combine, overtitle) })");
	rInside.parseEvalQ("jaspResultsModule$jaspTable$methods(addFootnote =   function(message='', symbol=NULL, col_names=NULL, row_names=NULL) { addFootnoteHelper(message, symbol, col_names, row_names) })");

	rInside["jasp.analyses"] = Rcpp::List();
	rInside.parseEvalQNT("suppressPackageStartupMessages(library('JASP'))");
	rInside.parseEvalQNT("suppressPackageStartupMessages(library('methods'))");
	rInside.parseEvalQNT("source(file='writeImage.R')");

	rinside->parseEvalNT("initEnvironment()");


}


const char* STDCALL jaspRCPP_run(const char* name, const char* title, bool requiresInit, const char* dataKey, const char* options, const char* resultsMeta, const char* stateKey, const char* perform, int ppi, int analysisID, int analysisRevision, bool usesJaspResults)
{
	SEXP results;

	RInside &rInside = rinside->instance();

	Rcpp::String jsonOptions = options;
	Rcpp::String jsonResultsMeta = resultsMeta;
	jsonOptions.set_encoding(Encoding);
	jsonResultsMeta.set_encoding(Encoding);


	rInside["name"]			= name;
	rInside["title"]		= title;
	rInside["requiresInit"]	= requiresInit;
	rInside["dataKey"]		= dataKey;
	rInside["options"]		= jsonOptions;
	rInside["resultsMeta"]	= jsonResultsMeta;
	rInside["stateKey"]		= stateKey;
	rInside["perform"]		= perform;
	rInside[".ppi"]			= ppi;

#ifndef __WIN32__
	rinside_consoleLog->clearConsoleBuffer();
#endif
	
	if(usesJaspResults)
	{
		///Some stuff for jaspResults etc
		jaspResults::setResponseData(analysisID, analysisRevision);
		jaspResults::setSaveLocation(jaspRCPP_requestJaspResultsRelativeFilePath());

		rInside.parseEval("runJaspResults(name=name, title=title, dataKey=dataKey, options=options, stateKey=stateKey)", results);
	}
	else
		rInside.parseEval("run(name=name, title=title, requiresInit=requiresInit, dataKey=dataKey, options=options, resultsMeta=resultsMeta, stateKey=stateKey, perform=perform)", results);

	static std::string str;
	if(Rcpp::is<std::string>(results))	str = Rcpp::as<std::string>(results);
	else								str = "error!";

	if(usesJaspResults)
	{
#ifdef PRINT_ENGINE_MESSAGES
		std::cout << "result of runJaspResults:\n" << str << std::endl << std::flush;
#endif
		jaspObject::destroyAllAllocatedObjects();
	}

	return str.c_str();
}

const char* STDCALL jaspRCPP_check()
{
	SEXP result = rinside->parseEvalNT("checkPackages()");
	static std::string staticResult;

	staticResult = Rf_isString(result) ? Rcpp::as<std::string>(result) : NullString;
	return staticResult.c_str();
}

void STDCALL jaspRCPP_runScript(const char * scriptCode)
{
	rinside->parseEvalNT(scriptCode);

	return;
}



int STDCALL jaspRCPP_runFilter(const char * filterCode, bool ** arrayPointer)
{
	lastErrorMessage = "";
	rinside->instance()[".filterCode"] = filterCode;
	const std::string filterTryCatch("\
		returnVal = 'null'; \
		tryCatch(\
			{ returnVal <- eval(parse(text=.filterCode)) }, \
			warning	= function(w) { .setRWarning(toString(w$message))	}, \
			error	= function(e) { .setRError(toString(e$message))	}\
		); \
		returnVal");
	SEXP result = rinside->parseEval(filterTryCatch);


	if(Rcpp::is<Rcpp::NumericVector>(result) || Rcpp::is<Rcpp::LogicalVector>(result))
	{
		Rcpp::NumericVector vec(result);

		if(vec.size() == 0)
			return 0;

		(*arrayPointer) = (bool*)malloc(vec.size() * sizeof(bool));

		for(int i=0; i<vec.size(); i++)
			(*arrayPointer)[i] = vec[i] == 1;

		return vec.size();
	}

	return -1;
}

void STDCALL jaspRCPP_resetErrorMsg()
{
	lastErrorMessage = "";
}

void STDCALL jaspRCPP_setErrorMsg(const char* msg)
{
	lastErrorMessage = msg;
}

const char*	STDCALL jaspRCPP_getLastErrorMsg()
{
	return lastErrorMessage.c_str();
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
	static std::string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<std::string>(result) : NullString;
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
	static std::string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<std::string>(result) : NullString;

	return staticResult.c_str();

}


const char*	STDCALL jaspRCPP_evalRCode(const char *rCode) {
	// Function to evaluate arbitrary R code from C++
	// Returns string if R result is a string, else returns "null"
	// Can also load the entire dataset if need be

	lastErrorMessage = "";
	rinside->instance()[".rCode"] = rCode;
	const std::string rCodeTryCatch(""
		"returnVal = 'null';	"
		"tryCatch(				"
		"	{	returnVal <- eval(parse(text=.rCode)) },						"
		"		warning	= function(w) { .setRWarning(toString(w$message))	},	"
		"		error	= function(e) { .setRError(toString(e$message))	}		"
		");			"
		"returnVal	");

	SEXP result = rinside->parseEvalNT(rCodeTryCatch);

	static std::string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<std::string>(result) : NullString;
	return staticResult.c_str();
}

} // extern "C"

#ifndef __WIN32__
const char* STDCALL jaspRCPP_getRConsoleOutput()
{
	static std::string output;
	output = rinside_consoleLog->getConsoleOutput();
	return output.c_str();
}
#endif

SEXP jaspRCPP_requestTempFileNameSEXP(SEXP extension)
{
	const char *root, *relativePath;
	std::string extensionAsString = Rcpp::as<std::string>(extension);

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


const char * jaspRCPP_requestJaspResultsRelativeFilePath()
{
	const char* root;
	const char* relativePath;

	if (!requestJaspResultsFileSourceCB(&root, &relativePath))
		return "";

	return relativePath;
}

SEXP jaspRCPP_requestStateFileNameSEXP()
{
	const char* root;
	const char* relativePath;

	if (!requestStateFileSourceCB(&root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"]			= root;
	paths["relativePath"]	= relativePath;

	return paths;
}


SEXP jaspRCPP_callbackSEXP(SEXP in, SEXP progress)
{
	std::string inStr = Rf_isNull(in) ? "null" : Rcpp::as<std::string>(in);
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

void jaspRCPP_setRWarning(SEXP Message)
{
	lastErrorMessage = "Warning: " + Rcpp::as<std::string>(Message);
}

void jaspRCPP_setRError(SEXP Message)
{
	lastErrorMessage = "Error: " + Rcpp::as<std::string>(Message);
}

void jaspRCPP_setColumnDataAsScale(std::string columnName,			Rcpp::Vector<REALSXP> scalarData)
{
	double scales[scalarData.size()];
	for(int i=0; i<scalarData.size(); i++)
		scales[i] = scalarData[i];

	dataSetColumnAsScale(columnName.c_str(), scales, scalarData.size());
}

void jaspRCPP_setColumnDataAsOrdinal(std::string columnName,		Rcpp::Vector<INTSXP> ordinalData)
{
	int ordinals[ordinalData.size()];
	for(int i=0; i<ordinalData.size(); i++)
		ordinals[i] = ordinalData[i];

	dataSetColumnAsOrdinal(columnName.c_str(), ordinals, ordinalData.size());
}

void jaspRCPP_setColumnDataAsNominal(std::string columnName,		Rcpp::Vector<INTSXP> nominalData)
{
	int nominals[nominalData.size()];
	for(int i=0; i<nominalData.size(); i++)
		nominals[i] = nominalData[i];

	dataSetColumnAsNominal(columnName.c_str(), nominals, nominalData.size());
}

void jaspRCPP_setColumnDataAsNominalText(std::string columnName,	Rcpp::Vector<STRSXP> nominalData)
{
	std::vector<std::string> convertedStrings(nominalData.begin(), nominalData.end());

	const char ** nominals = new const char*[convertedStrings.size()]();

	for(int i=0; i<convertedStrings.size(); i++)
		nominals[i] = convertedStrings[i].c_str();

	dataSetColumnAsNominalText(columnName.c_str(), nominals, nominalData.size());
}


RBridgeColumnType* jaspRCPP_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns, int *colMax)
{
	std::map<std::string, ColumnType> columnsRequested;

	if (Rf_isLogical(allColumns) && Rcpp::as<bool>(allColumns))
	{
		char** columns = readDataColumnNamesCB(colMax);
		if (columns)
		{
			for (int i = 0; i < *colMax; i++)
				columnsRequested[columns[i]] = ColumnTypeUnknown;
		}
	}

	auto setTypeRequested = [&columnsRequested](SEXP cols, ColumnType SetThis)
	{
		if(Rf_isString(cols))
		{
			std::vector<std::string> temp = Rcpp::as<std::vector<std::string>>(cols);
			for (size_t i = 0; i < temp.size(); i++)
				columnsRequested[temp.at(i)] = SetThis;
		}
	};

	setTypeRequested(columns,			ColumnTypeUnknown);
	setTypeRequested(columnsAsNumeric,	ColumnTypeScale);
	setTypeRequested(columnsAsOrdinal,	ColumnTypeOrdinal);
	setTypeRequested(columnsAsNominal,	ColumnTypeNominal);

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
		Rcpp::StringVector columnNames(colMax);

		for (int i = 0; i < colMax; i++)
		{
			RBridgeColumn& colResult = colResults[i];
			Rcpp::String colName = colResult.name;
			colName.set_encoding(Encoding);
			columnNames[i] = colName;

			if (colResult.isScale)
				list[i] = Rcpp::NumericVector(colResult.doubles, colResult.doubles + colResult.nbRows);
			else if(!colResult.hasLabels)
				list[i] = Rcpp::IntegerVector(colResult.ints, colResult.ints + colResult.nbRows);
			else
				list[i] = jaspRCPP_makeFactor(Rcpp::IntegerVector(colResult.ints, colResult.ints + colResult.nbRows), colResult.labels, colResult.nbLabels, colResult.isOrdinal);

		}

		list.attr("names")			= columnNames;
		dataFrame					= Rcpp::DataFrame(list);
		dataFrame.attr("row.names") = Rcpp::IntegerVector(colResults[colMax].ints, colResults[colMax].ints + colResults[colMax].nbRows);
	}

	return dataFrame;
}

Rcpp::DataFrame jaspRCPP_readDataSetHeaderSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	int colMax = 0;
	RBridgeColumnType* columnsRequested				= jaspRCPP_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns, &colMax);
	RBridgeColumnDescription* columnsDescription	= readDataSetDescriptionCB(columnsRequested, colMax);

	freeRBridgeColumnType(columnsRequested, colMax);

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame();

	if (columnsDescription)
	{
		Rcpp::List list(colMax);
		Rcpp::StringVector columnNames(colMax);

		for (int i = 0; i < colMax; i++)
		{
			RBridgeColumnDescription& colDescription = columnsDescription[i];
			Rcpp::String colName = colDescription.name;
			colName.set_encoding(Encoding);
			columnNames[i] = colName;

			if (colDescription.isScale)
				list(i) = Rcpp::NumericVector(0);
			else if (!colDescription.hasLabels)
				list(i) = Rcpp::IntegerVector(0);
			else
				list(i) = jaspRCPP_makeFactor(Rcpp::IntegerVector(0), colDescription.labels, colDescription.nbLabels, colDescription.isOrdinal);
		}

		list.attr("names") = columnNames;
		dataFrame = Rcpp::DataFrame(list);
	}

	return dataFrame;

}

Rcpp::IntegerVector jaspRCPP_makeFactor(Rcpp::IntegerVector v, char** levels, int nbLevels, bool ordinal)
{
	Rcpp::CharacterVector labels(nbLevels);
	for (int i = 0; i < nbLevels; i++)
	{
		Rcpp::String s = levels[i];
		s.set_encoding(Encoding);
		labels[i] = s;
	}


	v.attr("levels") = labels;

	std::vector<std::string> rClass;

	if (ordinal) rClass.push_back("ordered");
	rClass.push_back("factor");

	v.attr("class") = rClass;

	return v;
}
