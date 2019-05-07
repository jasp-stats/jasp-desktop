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
#include "jaspResults/src/jaspResults.h"



static const	std::string NullString = "null";
static			std::string lastErrorMessage = "";
static			cetype_t Encoding = CE_UTF8;

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

SetColumnAsScale			dataSetColumnAsScale;
SetColumnAsOrdinal			dataSetColumnAsOrdinal;
SetColumnAsNominal			dataSetColumnAsNominal;
SetColumnAsNominalText		dataSetColumnAsNominalText;

DataSetRowCount				dataSetRowCount;

static logFlushDef			_logFlushFunction		= nullptr;
static logWriteDef			_logWriteFunction		= nullptr;

extern "C" {
void STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks* callbacks,
	sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction,
	logFlushDef logFlushFunction, logWriteDef logWriteFunction)
{

	_logFlushFunction		= logFlushFunction;
	_logWriteFunction		= logWriteFunction;

	rinside = new RInside();

	RInside &rInside = rinside->instance();

	runCallbackCB							= callbacks->runCallbackCB;
	readDataSetCB							= callbacks->readDataSetCB;
	dataSetRowCount							= callbacks->dataSetRowCount;
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

	rInside[".dataSetRowCount"]				= Rcpp::InternalFunction(&jaspRCPP_dataSetRowCount);
	rInside[".setLog"]						= Rcpp::InternalFunction(&jaspRCPP_setLog);
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
	rInside[".createCaptureConnection"]		= Rcpp::InternalFunction(&jaspRCPP_CreateCaptureConnection);
	rInside[".requestTempFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestTempFileNameSEXP);
	rInside[".requestTempRootNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestTempRootNameSEXP);
	rInside[".setColumnDataAsNominalText"]	= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsNominalText);
	rInside[".requestStateFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestStateFileNameSEXP);

	rInside.parseEvalQNT(".outputSink <- .createCaptureConnection(); sink(.outputSink); print('.outputSink initialized!');");


	static const char *baseCitationFormat	= "JASP Team (%s). JASP (Version %s) [Computer software].";
	char baseCitation[200];
	sprintf(baseCitation, baseCitationFormat, buildYear, version);
	rInside[".baseCitation"]		= baseCitation;

	jaspResults::setSendFunc(sendToDesktopFunction);
	jaspResults::setPollMessagesFunc(pollMessagesFunction);
	jaspResults::setBaseCitation(baseCitation);
	jaspResults::setInsideJASP();

	rInside["jaspResultsModule"]			= givejaspResultsModule();

	//Adding some functions in R to the RefClass (generator) in the module
	jaspRCPP_parseEvalQNT("jaspResultsModule$jaspTable$methods(addColumnInfo = function(name=NULL, title=NULL, overtitle=NULL, type=NULL, format=NULL, combine=NULL) { addColumnInfoHelper(name, title, type, format, combine, overtitle) })");
	jaspRCPP_parseEvalQNT("jaspResultsModule$jaspTable$methods(addFootnote =   function(message='', symbol=NULL, col_names=NULL, row_names=NULL) { addFootnoteHelper(message, symbol, col_names, row_names) })");
	rInside["jasp.analyses"] = Rcpp::List();

	jaspRCPP_parseEvalQNT("suppressPackageStartupMessages(library(\"JASP\"))");
	jaspRCPP_parseEvalQNT("suppressPackageStartupMessages(library(\"JASPgraphs\"))");
	jaspRCPP_parseEvalQNT("suppressPackageStartupMessages(library(\"methods\"))");
	jaspRCPP_parseEvalQNT("suppressPackageStartupMessages(library(\"modules\"))");

	jaspRCPP_parseEvalQNT("source(file='writeImage.R')");
	jaspRCPP_parseEvalQNT("source(file='zzzWrappers.R')");

	jaspRCPP_parseEvalQNT("initEnvironment()");

	std::cout << "R_HOME: " << Rcpp::as<std::string>(rInside.parseEval("R.home('')")) << std::endl;
}

const char* STDCALL jaspRCPP_run(const char* name, const char* title, const char* rfile, bool requiresInit, const char* dataKey, const char* options, const char* resultsMeta, const char* stateKey, const char* perform, int ppi, int analysisID, int analysisRevision, bool usesJaspResults, const char* imageBackground)
{
	SEXP results;

	RInside &rInside = rinside->instance();

	Rcpp::String jsonOptions = options;
	Rcpp::String jsonResultsMeta = resultsMeta;
	jsonOptions.set_encoding(Encoding);
	jsonResultsMeta.set_encoding(Encoding);


	rInside["name"]				= name;
	rInside["title"]			= title;
	rInside["requiresInit"]		= requiresInit;
	rInside["dataKey"]			= dataKey;
	rInside["options"]			= jsonOptions;
	rInside["resultsMeta"]		= jsonResultsMeta;
	rInside["stateKey"]			= stateKey;
	rInside["perform"]			= perform;
	rInside[".ppi"]				= ppi;
	rInside[".imageBackground"]	= imageBackground;

	if (rfile && *rfile)
	{
		std::stringstream ss;
		ss << "loadNamespace(\"JASP\"); source(\"" << rfile << "\")";
		jaspRCPP_parseEvalQNT(ss.str());
	}

	if(usesJaspResults)
	{
		///Some stuff for jaspResults etc
		jaspResults::setResponseData(analysisID, analysisRevision);
		jaspResults::setSaveLocation(jaspRCPP_requestJaspResultsRelativeFilePath());

		results = jaspRCPP_parseEval("runJaspResults(name=name, title=title, dataKey=dataKey, options=options, stateKey=stateKey)");
	}
	else
		results = jaspRCPP_parseEval("run(name=name, title=title, requiresInit=requiresInit, dataKey=dataKey, options=options, resultsMeta=resultsMeta, stateKey=stateKey, perform=perform)");

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

const char* STDCALL jaspRCPP_runModuleCall(const char* name, const char* title, const char* moduleCall, const char* dataKey, const char* options, const char* stateKey, const char* perform, int ppi, int analysisID, int analysisRevision, const char* imageBackground)
{
	RInside &rInside				= rinside->instance();
	Rcpp::String jsonOptions		= options;
	jsonOptions.set_encoding(Encoding);


	rInside["name"]				= name;
	rInside["title"]			= title;
	rInside["requiresInit"]		= false;
	rInside["dataKey"]			= dataKey;
	rInside["options"]			= jsonOptions;
	rInside["resultsMeta"]		= "null";
	rInside["stateKey"]			= stateKey;
	rInside["perform"]			= perform;
	rInside["moduleCall"]		= moduleCall;
	rInside[".ppi"]				= ppi;
	rInside[".imageBackground"]	= imageBackground;

	jaspResults::setResponseData(analysisID, analysisRevision);
	jaspResults::setSaveLocation(jaspRCPP_requestJaspResultsRelativeFilePath());

	SEXP results = jaspRCPP_parseEval("runJaspResults(name=name, title=title, dataKey=dataKey, options=options, stateKey=stateKey, functionCall=moduleCall)");

	static std::string str;
	if(Rcpp::is<std::string>(results))	str = Rcpp::as<std::string>(results);
	else								str = "error!";


#ifdef PRINT_ENGINE_MESSAGES
	std::cout << "result of runJaspResults:\n" << str << std::endl << std::flush;
#endif

	jaspObject::destroyAllAllocatedObjects();

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
	jaspRCPP_parseEvalQNT(scriptCode);

	return;
}

const char * STDCALL jaspRCPP_runScriptReturnString(const char * scriptCode)
{
	static std::string returnStr;
	returnStr = Rcpp::as<std::string>(jaspRCPP_parseEval(scriptCode));

	return returnStr.c_str();
}



int STDCALL jaspRCPP_runFilter(const char * filterCode, bool ** arrayPointer)
{
	jaspRCPP_logString(std::string("jaspRCPP_runFilter runs: \n\"") + filterCode + "\"\n" );

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
	SEXP result = jaspRCPP_parseEval(filterTryCatch);


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

const char* STDCALL jaspRCPP_saveImage(const char *name, const char *type, const int height, const int width, const int ppi, const char* imageBackground)
{
	RInside &rInside = rinside->instance();

	rInside["plotName"]			= name;
	rInside["format"]			= type;

	rInside["height"]			= height;
	rInside["width"]			= width;
	rInside[".ppi"]				= ppi;
	rInside[".imageBackground"] = imageBackground;

	SEXP result = jaspRCPP_parseEval("saveImage(plotName,format,height,width)");
	static std::string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<std::string>(result) : NullString;
	return staticResult.c_str();
}

const char* STDCALL jaspRCPP_editImage(const char *name, const char *type, const int height, const int width, const int ppi, const char* imageBackground)
{

	RInside &rInside = rinside->instance();

	rInside["plotName"]			= name;
	rInside["type"]				= type;
	rInside["height"]			= height;
	rInside["width"]			= width;
	rInside[".ppi"]				= ppi;
	rInside[".imageBackground"] = imageBackground;

	SEXP result = jaspRCPP_parseEval("editImage(plotName,type,height,width)");
	static std::string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<std::string>(result) : NullString;

	return staticResult.c_str();

}


void STDCALL jaspRCPP_rewriteImages(const int ppi, const char* imageBackground) {

	RInside &rInside = rinside->instance();

	rInside[".ppi"]				= ppi;
	rInside[".imageBackground"] = imageBackground;

	jaspRCPP_parseEvalQNT("rewriteImages()");
}


const char*	STDCALL jaspRCPP_evalRCode(const char *rCode) {
	// Function to evaluate arbitrary R code from C++
	// Returns string if R result is a string, else returns "null"
	// Can also load the entire dataset if need be


	jaspRCPP_logString(std::string("jaspRCPP_evalRCode runs: \n\"") + rCode + "\"\n" );

	lastErrorMessage = "";
	rinside->instance()[".rCode"] = rCode;
	const std::string rCodeTryCatch(""
		"returnVal = 'null';	"
		"tryCatch(				"
		"	suppressWarnings({	returnVal <- eval(parse(text=.rCode)) }),		"
		"		error	= function(e) { .setRError(toString(e$message))	}		"
		");			"
		"returnVal	");

	SEXP result = jaspRCPP_parseEval(rCodeTryCatch);

	static std::string staticResult;
	staticResult = Rf_isString(result) ? Rcpp::as<std::string>(result) : NullString;
	return staticResult.c_str();
}

} // extern "C"

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
	std::string inStr	= Rf_isNull(in)			? "null"	: Rcpp::as<std::string>(in);
	int progressInt		= Rf_isNull(progress)	? -1		: Rcpp::as<int>(progress);
	const char *out;

	return runCallbackCB(inStr.c_str(), progressInt, &out) ? Rcpp::CharacterVector(out) : 0;
}

void jaspRCPP_returnDataFrame(Rcpp::DataFrame frame)
{
	long colcount = frame.size();

	std::cout << "got a dataframe!\n" << colcount << "X" << (colcount > 0 ? Rcpp::as<Rcpp::NumericVector>(frame[0]).size() : -1) << "\n" << std::flush;

	if(colcount > 0)
	{
		long rowcount = Rcpp::as<Rcpp::NumericVector>(frame[0]).size();

		for(long row=0; row<rowcount; row++)
		{
			for(long col=0; col<colcount; col++)
				std::cout << "'" << Rcpp::as<Rcpp::StringVector>(frame[col])[row] << " or " <<  Rcpp::as<Rcpp::NumericVector>(frame[col])[row]  << "'\t" << std::flush;

			std::cout << "\n";
		}
		std::cout << std::flush;
	}
}

void jaspRCPP_returnString(SEXP Message)
{
	std::cout << "A message from R: " << static_cast<std::string>(Rcpp::as<Rcpp::String>(Message)) << "\n" << std::flush;
}

void jaspRCPP_setRWarning(SEXP Message)
{
	lastErrorMessage = "Warning: " + Rcpp::as<std::string>(Message);
}

void jaspRCPP_setRError(SEXP Message)
{
	lastErrorMessage = "Error: " + Rcpp::as<std::string>(Message);
}

void jaspRCPP_setLog(SEXP Message)
{
	lastErrorMessage = Rcpp::as<std::string>(Message);
	std::cout << "jaspRCPP_setLog receives: '" << lastErrorMessage << "'" << std::endl;
}

int jaspRCPP_dataSetRowCount()
{
	return dataSetRowCount();
}

bool jaspRCPP_setColumnDataAsScale(std::string columnName, Rcpp::RObject scalarData)
{
	if(Rcpp::is<Rcpp::Vector<REALSXP>>(scalarData))
		return _jaspRCPP_setColumnDataAsScale(columnName, Rcpp::as<Rcpp::Vector<REALSXP>>(scalarData));

	(*rinside)["jaspRCPP_setColumnDataAsScaleData"] = scalarData;
	//rinside->parseEvalNT("jaspRCPP_setColumnDataAsScaleData");
	Rcpp::RObject result = jaspRCPP_parseEval("suppressWarnings(as.numeric(as.character(jaspRCPP_setColumnDataAsScaleData)))");

	if(Rcpp::is<Rcpp::Vector<REALSXP>>(result))
		return _jaspRCPP_setColumnDataAsScale(columnName, Rcpp::as<Rcpp::Vector<REALSXP>>(result));

	Rf_error("Something went wrong with the conversion to scalar..");
}

bool _jaspRCPP_setColumnDataAsScale(std::string columnName, Rcpp::Vector<REALSXP> scalarData)
{
	double *scales = new double[scalarData.size()];
	for(int i=0; i<scalarData.size(); i++)
		scales[i] = scalarData[i];

	bool somethingChanged = dataSetColumnAsScale(columnName.c_str(), scales, static_cast<size_t>(scalarData.size()));

	delete[] scales;

	return somethingChanged;
}


bool jaspRCPP_setColumnDataAsOrdinal(std::string columnName, Rcpp::RObject ordinalData)
{
	if(Rcpp::is<Rcpp::Vector<INTSXP>>(ordinalData))
		return _jaspRCPP_setColumnDataAsOrdinal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(ordinalData));

	(*rinside)["jaspRCPP_setColumnDataAsOrdinalData"] = ordinalData;
	//rinside->parseEvalNT("jaspRCPP_setColumnDataAsOrdinalData");
	Rcpp::RObject result = jaspRCPP_parseEval("suppressWarnings(as.factor(as.character(jaspRCPP_setColumnDataAsOrdinalData)))");

	if(Rcpp::is<Rcpp::Vector<INTSXP>>(result))
		return _jaspRCPP_setColumnDataAsOrdinal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(result));

	Rf_error("Something went wrong with the conversion to ordinal..");
}

bool _jaspRCPP_setColumnDataAsOrdinal(std::string columnName, Rcpp::Vector<INTSXP> ordinalData)
{
	int *ordinals = new int[ordinalData.size()];
	for(int i=0; i<ordinalData.size(); i++)
		ordinals[i] = ordinalData[i];

	size_t			numLevels;
	const char **	labelPointers;
	std::string *	labels;

	jaspRCPP_setColumnDataHelper_FactorsLevels(ordinalData, ordinals, numLevels, labelPointers, labels);

	bool somethingChanged =  dataSetColumnAsOrdinal(columnName.c_str(), ordinals, static_cast<size_t>(ordinalData.size()), labelPointers, numLevels);

	delete[] ordinals;
	delete[] labels;
	delete[] labelPointers;

	return somethingChanged;
}

bool jaspRCPP_setColumnDataAsNominal(std::string columnName, Rcpp::RObject nominalData)
{
	if(Rcpp::is<Rcpp::Vector<INTSXP>>(nominalData))
		return _jaspRCPP_setColumnDataAsNominal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(nominalData));

	(*rinside)["jaspRCPP_setColumnDataAsNominalData"] = nominalData;
	//rinside->parseEvalNT("jaspRCPP_setColumnDataAsNominalData");
	Rcpp::RObject result = jaspRCPP_parseEval("suppressWarnings(as.factor(as.character(jaspRCPP_setColumnDataAsNominalData)))");

	if(Rcpp::is<Rcpp::Vector<INTSXP>>(result))
		return _jaspRCPP_setColumnDataAsNominal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(result));

	Rf_error("Something went wrong with the conversion to nominal..");
}

bool _jaspRCPP_setColumnDataAsNominal(std::string columnName, Rcpp::Vector<INTSXP> nominalData)
{
	int *nominals = new int[nominalData.size()];
	for(int i=0; i<nominalData.size(); i++)
		nominals[i] = nominalData[i];

	size_t			numLevels;
	const char **	labelPointers;
	std::string *	labels;

	jaspRCPP_setColumnDataHelper_FactorsLevels(nominalData, nominals, numLevels, labelPointers, labels);

	bool somethingChanged =  dataSetColumnAsNominal(columnName.c_str(), nominals, static_cast<size_t>(nominalData.size()), labelPointers, numLevels);

	delete[] nominals;
	delete[] labels;
	delete[] labelPointers;

	return somethingChanged;
}

bool jaspRCPP_setColumnDataAsNominalText(std::string columnName, Rcpp::RObject nominalData)
{
	if(Rf_isNull(nominalData))
		return _jaspRCPP_setColumnDataAsNominalText(columnName, Rcpp::Vector<STRSXP>());

	return _jaspRCPP_setColumnDataAsNominalText(columnName, Rcpp::as<Rcpp::Vector<STRSXP>>(nominalData));
}


bool _jaspRCPP_setColumnDataAsNominalText(std::string columnName, Rcpp::Vector<STRSXP> nominalData)
{
	std::vector<std::string> convertedStrings(nominalData.begin(), nominalData.end());

	const char ** nominals = new const char*[convertedStrings.size()]();

	for(size_t i=0; i<convertedStrings.size(); i++)
		nominals[i] = convertedStrings[i].c_str();

	return dataSetColumnAsNominalText(columnName.c_str(), nominals, static_cast<size_t>(nominalData.size()));
}


void jaspRCPP_setColumnDataHelper_FactorsLevels(Rcpp::Vector<INTSXP> data, int *& outputData, size_t & numLevels, const char **& labelPointers, std::string *& labels)
{
	Rcpp::CharacterVector	levels;
	numLevels = 0;

	if(!Rf_isNull(data.attr("levels")))
	{
		levels = data.attr("levels");
		numLevels = size_t(levels.size());
	}

	if(numLevels > 0)
	{
		labels			= new std::string [numLevels];
		labelPointers	= new const char * [numLevels];

		for(int i=0; i<numLevels; i++)
		{
			labels[i]			= levels[i];
			labelPointers[i]	= labels[i].c_str();
		}
	}
	else
	{
		std::set<int> unique;
		for(int i=0; i<data.size(); i++)
			unique.insert(outputData[i]);
		numLevels = unique.size();

		std::vector<int> sorted(unique.begin(), unique.end());
		std::sort(sorted.begin(), sorted.end());

		labels			= new std::string [numLevels];
		labelPointers	= new const char * [numLevels];

		std::map<std::string, int> levelToVal;

		for(size_t i=0; i<sorted.size(); i++)
		{
			labels[i]				= std::to_string(sorted[i]);
			labelPointers[i]		= labels[i].c_str();
			levelToVal[labels[i]]	= i + 1;
		}

		for(int i=0; i<data.size(); i++)
			outputData[i] = levelToVal[std::to_string(outputData[i])];;
	}
}


RBridgeColumnType* jaspRCPP_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns, size_t * colMax)
{
	std::map<std::string, ColumnType> columnsRequested;

	if (Rf_isLogical(allColumns) && Rcpp::as<bool>(allColumns))
	{
		char** columns = readDataColumnNamesCB(colMax);
		if (columns)
		{
			for (size_t i = 0; i < *colMax; i++)
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

	RBridgeColumnType* result = static_cast<RBridgeColumnType*>(calloc(columnsRequested.size(), sizeof(RBridgeColumnType)));
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

void freeRBridgeColumnType(RBridgeColumnType *columns, size_t colMax)
{
	for (int i = 0; i < colMax; i++)
		free(columns[i].name);

	free(columns);
}

Rcpp::DataFrame jaspRCPP_readFullDataSet()
{
	size_t colMax = 0;
	RBridgeColumn* colResults = readFullDataSetCB(&colMax);
	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_readFilterDataSet()
{
	size_t colMax = 0;
	RBridgeColumn* colResults = readFilterDataSetCB(&colMax);

	if(colMax == 0)
		return Rcpp::DataFrame();

	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_readDataSetSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	size_t colMax = 0;
	RBridgeColumnType* columnsRequested = jaspRCPP_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns, &colMax);
	RBridgeColumn* colResults = readDataSetCB(columnsRequested, colMax, true);
	freeRBridgeColumnType(columnsRequested, colMax);

	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_convertRBridgeColumns_to_DataFrame(const RBridgeColumn* colResults, size_t colMax)
{
	Rcpp::DataFrame dataFrame = Rcpp::DataFrame();

	if (colResults)
	{
		Rcpp::List list(colMax);
		Rcpp::StringVector columnNames(colMax);

		for (int i = 0; i < colMax; i++)
		{
			const RBridgeColumn& colResult = colResults[i];
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
	size_t colMax = 0;
	RBridgeColumnType* columnsRequested				= jaspRCPP_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns, &colMax);
	RBridgeColumnDescription* columnsDescription	= readDataSetDescriptionCB(columnsRequested, colMax);

	freeRBridgeColumnType(columnsRequested, colMax);

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame();

	if (columnsDescription)
	{
		Rcpp::List list(colMax);
		Rcpp::StringVector columnNames(colMax);

		for (size_t i = 0; i < colMax; i++)
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
/*#ifdef JASP_DEBUG
	std::cout << "jaspRCPP_makeFactor:\n\tlevels:\n\t\tnum: " << nbLevels << "\n\t\tstrs:\n";
	for(int i=0; i<nbLevels; i++)
		std::cout << "\t\t\t'" << levels[i] << "'\n";
	std::cout << "intVec: ";

	for(int i=0; i<v.size(); i++)
		std::cout << v[i] << (i < v.size() - 1 ? ", " : "" );
	std::cout << std::endl;
#endif*/

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

	if(v.size() == 0)
		return v;

	static Rcpp::Function droplevels("droplevels");
	return droplevels(Rcpp::_["x"] = v);
}

struct jaspRCPP_Connection
{
	static Rboolean	open(struct Rconn *)		{ return Rboolean::TRUE;	}
	static void		close(struct Rconn *)		{}
	static void		destroy(struct Rconn *)		{}
	static int		fflush(struct Rconn *)		{ std::cout << std::flush; return 0;	}

	static size_t	write(const void * buf, size_t, size_t len, struct Rconn * = nullptr)
	{
		return _logWriteFunction(buf, len);
	}

	static int vfprintf(struct Rconn *, const char * format, va_list args)
	{
		const int maxChar = 1024 * 1024 * 30; //30MB should be enough for any crazy stuff right?
		static std::vector<char> buf(maxChar);

		int l = std::vsnprintf(buf.data(), maxChar, format, args);

		write(buf.data(), 0, l);

		return l;
	}
};


void jaspRCPP_logString(const std::string & logThis)
{
	jaspRCPP_Connection::write(logThis.c_str(), 0, logThis.size(), nullptr);
}

void jaspRCPP_parseEvalPreface(const std::string & code)
{
	jaspRCPP_logString("Evaluating R-code:\n");
	jaspRCPP_Connection::write(code.c_str(), 0, code.size(), nullptr);
	jaspRCPP_logString("\nOutput:\n");
}

std::string __sinkMe(const std::string & code)
{
	return "sink(.outputSink, type='output');\nsink(.outputSink, type='message');\n" + code;
}

void jaspRCPP_parseEvalQNT(const std::string & code)
{
	jaspRCPP_parseEvalPreface(code);
	rinside->parseEvalQNT(__sinkMe(code));
	jaspRCPP_logString("\n");
}

RInside::Proxy jaspRCPP_parseEval(const std::string & code)
{
	jaspRCPP_parseEvalPreface(code);
	return rinside->parseEval(__sinkMe(code));
	jaspRCPP_logString("\n");
}

extern "C" {
//We need to to the following crazy defines to make sure the header actually gets accepted by the compiler...
#define class _class
#define private _private;
#include "R_ext/Connections.h"
}

SEXP jaspRCPP_CreateCaptureConnection()
{
	Rconnection con;

	SEXP rc = PROTECT(R_new_custom_connection("jaspRCPP_OUT", "w", "jaspRCPP_OUT", &con));

	/* set connection properties */
	con->incomplete		= FALSE;
	//con->private		= req;
	con->canseek		= FALSE;
	con->canwrite		= TRUE;
	con->isopen			= TRUE;
	con->blocking		= TRUE;
	con->text			= TRUE;
	con->UTF8out		= TRUE;
	con->open			= jaspRCPP_Connection::open;
	con->close			= jaspRCPP_Connection::close;
	con->destroy		= jaspRCPP_Connection::destroy;
	con->fflush			= jaspRCPP_Connection::fflush;
	con->write			= jaspRCPP_Connection::write;
	con->vfprintf		= jaspRCPP_Connection::vfprintf;

	UNPROTECT(1);
	return rc;
}
