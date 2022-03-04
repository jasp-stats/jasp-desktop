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
#include <fstream>
// #include "boost/nowide/system.hpp"

static const	std::string NullString			= "null";
static			std::string lastErrorMessage	= "";
static			cetype_t	Encoding			= CE_UTF8;

//Ok so CSTRING_TO_R could actually be replaced by std::string and everything would keep functioning regardless as far as I can see. But, in case later we want to change the encoding more drastically this gives an issue startpoint. In that case one should also do something about jaspRCPP_parseEval & jaspRCPP_parseEvalQNT (which will lead to horrible looking code ;) )
#define CSTRING_TO_R_CHARSXP(constchar) Rf_mkCharCE(constchar, Encoding)
#define CSTRING_TO_R(constchar) Rf_ScalarString(CSTRING_TO_R_CHARSXP(constchar))

#define CSTRING_TO_R_CHARSXP_UTF8(constchar) Rf_mkCharCE(constchar, CE_UTF8)
#define CSTRING_TO_R_UTF8(constchar) Rf_ScalarString(CSTRING_TO_R_CHARSXP_UTF8(constchar))

RInside							*rinside;
ReadDataSetCB					readDataSetCB;
RunCallbackCB					runCallbackCB;
ReadADataSetCB					readFullDataSetCB,
								readFullFilteredDataSetCB,
								readFilterDataSetCB;
ReadDataColumnNamesCB			readDataColumnNamesCB;
RequestTempFileNameCB			requestTempFileNameCB,
								requestSpecificFileNameCB;
RequestTempRootNameCB			requestTempRootNameCB;
ReadDataSetDescriptionCB		readDataSetDescriptionCB;
RequestPredefinedFileSourceCB	requestStateFileSourceCB,
								requestJaspResultsFileSourceCB;

GetColumnType					dataSetGetColumnType;
SetColumnAsScale				dataSetColumnAsScale;
SetColumnAsOrdinal				dataSetColumnAsOrdinal;
SetColumnAsNominal				dataSetColumnAsNominal;
SetColumnAsNominalText			dataSetColumnAsNominalText;

DataSetRowCount					dataSetRowCount;

EnDecodeDef						encodeColumnName,
								decodeColumnName,
								encodeAllColumnNames,
								decodeAllColumnNames;

getColNames						getAllColumnNames;

static logFlushDef				_logFlushFunction		= nullptr;
static logWriteDef				_logWriteFunction		= nullptr;
static sendFuncDef				_sendToDesktop			= nullptr;
static systemDef				_systemFunc				= nullptr;
static libraryFixerDef			_libraryFixerFunc		= nullptr;
static EnDecodeDef				_stringNativeToUtf8		= nullptr;

static std::string				_R_HOME = "";

bool shouldCrashSoon = false; //Simply here to allow a developer to force a crash

extern "C" {
void STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks* callbacks,
	sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction,
	logFlushDef logFlushFunction, logWriteDef logWriteFunction,
	systemDef systemFunc, libraryFixerDef libraryFixerFunc, const char* resultsFont,
	EnDecodeDef nativeToUtf8)
{
	_logFlushFunction		= logFlushFunction;
	_logWriteFunction		= logWriteFunction;
	_sendToDesktop			= sendToDesktopFunction;
	_systemFunc				= systemFunc;
	_libraryFixerFunc		= libraryFixerFunc;
	_stringNativeToUtf8		= nativeToUtf8;

	jaspRCPP_logString("Creating RInside.\n");
	rinside = new RInside();
	
	RInside &rInside = rinside->instance();

	requestJaspResultsFileSourceCB				= callbacks->requestJaspResultsFileSourceCB;
	dataSetColumnAsNominalText					= callbacks->dataSetColumnAsNominalText;
	requestSpecificFileNameCB					= callbacks->requestSpecificFileNameCB;
	readFullFilteredDataSetCB					= callbacks->readFullFilteredDataSetCB;
	requestStateFileSourceCB					= callbacks->requestStateFileSourceCB;
	readDataSetDescriptionCB					= callbacks->readDataSetDescriptionCB;
	dataSetColumnAsNominal						= callbacks->dataSetColumnAsNominal;
	dataSetColumnAsOrdinal						= callbacks->dataSetColumnAsOrdinal;
	requestTempRootNameCB						= callbacks->requestTempRootNameCB;
	requestTempFileNameCB						= callbacks->requestTempFileNameCB;
	readDataColumnNamesCB						= callbacks->readDataColumnNamesCB;
	dataSetColumnAsScale						= callbacks->dataSetColumnAsScale;
	dataSetGetColumnType						= callbacks->dataSetGetColumnType;
	encodeAllColumnNames						= callbacks->encoderAll;
	decodeAllColumnNames						= callbacks->decoderAll;
	readFilterDataSetCB							= callbacks->readFilterDataSetCB;
	readFullDataSetCB							= callbacks->readFullDataSetCB;
	getAllColumnNames							= callbacks->columnNames;
	encodeColumnName							= callbacks->encoder;
	decodeColumnName							= callbacks->decoder;
	dataSetRowCount								= callbacks->dataSetRowCount;
	runCallbackCB								= callbacks->runCallbackCB;
	readDataSetCB								= callbacks->readDataSetCB;


	rInside[".setLog"]							= Rcpp::InternalFunction(&jaspRCPP_setLog);
	rInside[".setRError"]						= Rcpp::InternalFunction(&jaspRCPP_setRError);
	rInside[".crashPlease"]						= Rcpp::InternalFunction(&jaspRCPP_crashPlease);
	rInside[".setRWarning"]						= Rcpp::InternalFunction(&jaspRCPP_setRWarning);
	rInside[".runSeparateR"]					= Rcpp::InternalFunction(&jaspRCPP_RunSeparateR);
	rInside[".returnString"]					= Rcpp::InternalFunction(&jaspRCPP_returnString);
	rInside[".columnIsScale"]					= Rcpp::InternalFunction(&jaspRCPP_columnIsScale);
	rInside[".callbackNative"]					= Rcpp::InternalFunction(&jaspRCPP_callbackSEXP);
	rInside[".dataSetRowCount"]					= Rcpp::InternalFunction(&jaspRCPP_dataSetRowCount);
	rInside[".returnDataFrame"]					= Rcpp::InternalFunction(&jaspRCPP_returnDataFrame);
	rInside[".columnIsOrdinal"]					= Rcpp::InternalFunction(&jaspRCPP_columnIsOrdinal);
	rInside[".columnIsNominal"]					= Rcpp::InternalFunction(&jaspRCPP_columnIsNominal);
	rInside[".encodeColNamesLax"]				= Rcpp::InternalFunction(&jaspRCPP_encodeAllColumnNames);
	rInside[".decodeColNamesLax"]				= Rcpp::InternalFunction(&jaspRCPP_decodeAllColumnNames);
	rInside[".columnIsNominalText"]				= Rcpp::InternalFunction(&jaspRCPP_columnIsNominalText);
	rInside[".encodeColNamesStrict"]			= Rcpp::InternalFunction(&jaspRCPP_encodeColumnName);
	rInside[".decodeColNamesStrict"]			= Rcpp::InternalFunction(&jaspRCPP_decodeColumnName);
	rInside[".setColumnDataAsScale"]			= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsScale);
	rInside[".readFullDatasetToEnd"]			= Rcpp::InternalFunction(&jaspRCPP_readFullDataSet);
	rInside[".allColumnNamesDataset"]			= Rcpp::InternalFunction(&jaspRCPP_allColumnNamesDataset);
	rInside[".readDatasetToEndNative"]			= Rcpp::InternalFunction(&jaspRCPP_readDataSetSEXP);
	rInside[".readFilterDatasetToEnd"]			= Rcpp::InternalFunction(&jaspRCPP_readFilterDataSet);
	rInside[".setColumnDataAsOrdinal"]			= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsOrdinal);
	rInside[".setColumnDataAsNominal"]			= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsNominal);
	rInside[".readDataSetHeaderNative"]			= Rcpp::InternalFunction(&jaspRCPP_readDataSetHeaderSEXP);
	rInside[".createCaptureConnection"]			= Rcpp::InternalFunction(&jaspRCPP_CreateCaptureConnection);
	rInside[".postProcessLibraryModule"]		= Rcpp::InternalFunction(&jaspRCPP_postProcessLocalPackageInstall);
	rInside[".requestTempFileNameNative"]		= Rcpp::InternalFunction(&jaspRCPP_requestTempFileNameSEXP);
	rInside[".requestTempRootNameNative"]		= Rcpp::InternalFunction(&jaspRCPP_requestTempRootNameSEXP);
	rInside[".setColumnDataAsNominalText"]		= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsNominalText);
	rInside[".requestStateFileNameNative"]		= Rcpp::InternalFunction(&jaspRCPP_requestStateFileNameSEXP);
	rInside[".readFullFilteredDatasetToEnd"]	= Rcpp::InternalFunction(&jaspRCPP_readFullFilteredDataSet);
	rInside[".requestSpecificFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestSpecificFileNameSEXP);
	
	jaspRCPP_logString("Creating Output sink.\n");
	rInside.parseEvalQNT(".outputSink <- .createCaptureConnection(); sink(.outputSink); print('.outputSink initialized!'); sink();");

	static const char *baseCitationFormat	= "JASP Team (%s). JASP (Version %s) [Computer software].";
	char baseCitation[200];
	sprintf(baseCitation, baseCitationFormat, buildYear, version);
	rInside[".baseCitation"]		= CSTRING_TO_R(baseCitation);
	rInside[".resultsFont"]			= resultsFont;

	jaspResults::setSendFunc(sendToDesktopFunction);
	jaspResults::setPollMessagesFunc(pollMessagesFunction);
	jaspResults::setBaseCitation(baseCitation);
	jaspResults::setInsideJASP();

	jaspRCPP_parseEvalQNT("library(methods)");
	jaspRCPP_parseEvalQNT("library(jaspBase)");

	jaspRCPP_logString("Initializing jaspResultsModule.\n");

	rInside["jaspResultsModule"]			= givejaspResultsModule();

	//Adding some functions in R to the RefClass (generator) in the module
	//To do: move this entirely to zzzWrapper if this wasn't done yet.
	jaspRCPP_parseEvalQNT("jaspResultsModule$jaspTable$methods(addColumnInfo = function(name=NULL, title=NULL, overtitle=NULL, type=NULL, format=NULL, combine=NULL) { addColumnInfoHelper(name, title, type, format, combine, overtitle) })");
	jaspRCPP_parseEvalQNT("jaspResultsModule$jaspTable$methods(addFootnote =   function(message='', symbol=NULL, col_names=NULL, row_names=NULL) { addFootnoteHelper(message, symbol, col_names, row_names) })");
	
	jaspRCPP_logString("Initializing jaspBase.\n");
	jaspRCPP_parseEvalQNT("library(\"jaspBase\")");
		
	jaspRCPP_logString("Loading auxillary R-files.\n");

#if defined _WIN32 || defined __MINGW32__
	jaspRCPP_parseEvalQNT("source(file='Modules/writeImage.R')");
	jaspRCPP_parseEvalQNT("source(file='Modules/zzzWrappers.R')");
	jaspRCPP_parseEvalQNT("source(file='Modules/workarounds.R')");
#else
	jaspRCPP_parseEvalQNT("source(file='../Modules/writeImage.R')");
	jaspRCPP_parseEvalQNT("source(file='../Modules/zzzWrappers.R')");
	jaspRCPP_parseEvalQNT("source(file='../Modules/workarounds.R')");
#endif

	jaspRCPP_logString("initEnvironment().\n");
	jaspRCPP_parseEvalQNT("initEnvironment()");
	
	_R_HOME = jaspRCPP_parseEvalStringReturn("R.home('')");
	jaspRCPP_logString("R_HOME is: " + _R_HOME + "\n");
	

#ifdef __APPLE__
	/*This won't actually work because rjags and runjags are not part of the standard library... See https://github.com/jasp-stats/INTERNAL-jasp/issues/1345
	jaspRCPP_parseEvalQNT("options(jags.moddir=paste0(Sys.getenv('JAGS_HOME'),'/modules-4'))");
	jaspRCPP_parseEvalQNT("suppressPackageStartupMessages(library(\"rjags\"))");
	jaspRCPP_parseEvalQNT("suppressPackageStartupMessages(library(\"runjags\"))");
	jaspRCPP_parseEvalQNT("runjags::runjags.options(jagspath=Sys.getenv('JAGS_HOME'))");*/
#endif

	jaspRCPP_logString("initializeDoNotRemoveList().\n");
	jaspRCPP_parseEvalQNT("jaspBase:::.initializeDoNotRemoveList()");
}

void STDCALL jaspRCPP_junctionHelper(bool collectNotRestore, const char * folder)
{
	rinside = new RInside();
	RInside &rInside = rinside->instance();
	
	std::cout << "RInside created, now about to " << (collectNotRestore ? "collect" :  "recreate") << " Modules junctions in renv-cache" << std::endl;
	
	rinside->parseEvalQNT("source('../Modules/symlinkTools.R')");
	
	rInside["symFolder"] = CSTRING_TO_R_UTF8(folder);
	
	if(collectNotRestore)	rinside->parseEvalQNT("collectAndStoreJunctions(symFolder)");
	else					rinside->parseEvalQNT("restoreModulesIfNeeded(  symFolder)");
}

void STDCALL jaspRCPP_purgeGlobalEnvironment()
{
	jaspRCPP_parseEvalQNT("jaspBase:::.cleanEngineMemory()", false);
}

void _setJaspResultsInfo(int analysisID, int analysisRevision, bool developerMode)
{
	jaspResults::setResponseData(analysisID, analysisRevision);
	jaspResults::setDeveloperMode(developerMode);

	std::string root, relativePath;

	if(!jaspRCPP_requestJaspResultsRelativeFilePath(root, relativePath))
		throw std::runtime_error("Did not receive a valid filename to store jaspResults.json at.");

	jaspResults::setSaveLocation(root, relativePath);

	if(!jaspRCPP_requestSpecificRelativeFilePath(jaspResults::writeSealFilename(), root, relativePath))
			throw std::runtime_error("Did not receive a valid filename to store jaspResults write seal at.");

	jaspResults::setWriteSealLocation(root, relativePath);
}

const char* STDCALL jaspRCPP_runModuleCall(const char* name, const char* title, const char* moduleCall, const char* dataKey, const char* options, const char* stateKey, int ppi, int analysisID, int analysisRevision, const char* imageBackground, bool developerMode, const char* resultsFont)
{
	RInside &rInside			= rinside->instance();

	rInside["name"]				= CSTRING_TO_R(name);
	rInside["title"]			= CSTRING_TO_R(title);
	rInside["options"]			= CSTRING_TO_R(options);
	rInside[".ppi"]				= ppi;
	rInside["dataKey"]			= dataKey;
	rInside["stateKey"]			= stateKey;
	rInside["moduleCall"]		= moduleCall;
	rInside["resultsMeta"]		= "null";
	rInside["requiresInit"]		= false;
	rInside[".imageBackground"]	= imageBackground;
	rInside[".resultsFont"]		= resultsFont;

	_setJaspResultsInfo(analysisID, analysisRevision, developerMode);

	SEXP results = jaspRCPP_parseEval("runJaspResults(name=name, title=title, dataKey=dataKey, options=options, stateKey=stateKey, functionCall=moduleCall)", true);

	static std::string str;
	if(results != NULL && Rcpp::is<std::string>(results))	str = jaspNativeToUtf8(Rcpp::as<Rcpp::String>(results));
	else													str = "error!";


#ifdef PRINT_ENGINE_MESSAGES
	jaspRCPP_logString("result of runJaspResults:\n" + str + "\n");
#endif

	jaspObject::destroyAllAllocatedObjects();

	jaspRCPP_checkForCrashRequest();

	return str.c_str();
}

void STDCALL jaspRCPP_runScript(const char * scriptCode)
{
	jaspRCPP_parseEvalQNT(scriptCode);

	jaspRCPP_checkForCrashRequest();

	return;
}

const char * STDCALL jaspRCPP_runScriptReturnString(const char * scriptCode)
{
	static std::string returnStr;
	std::string script(scriptCode);
	returnStr = jaspRCPP_parseEvalStringReturn(script);

	jaspRCPP_checkForCrashRequest();

	return returnStr.c_str();
}

int STDCALL jaspRCPP_runFilter(const char * filterCode, bool ** arrayPointer)
{
	jaspRCPP_logString("jaspRCPP_runFilter runs: \n\"" + std::string(filterCode) + "\"\n" );

	lastErrorMessage = "";
	rinside->instance()[".filterCode"] = CSTRING_TO_R(filterCode);

	const std::string filterTryCatch(
		"returnVal = 'null'; \n"
		"print(paste0('Running filtercode: ', .filterCode)); \n"
		"tryCatch(\n"
		"	{ returnVal <- eval(parse(text=.filterCode)) }, \n"
		"	warning	= function(w) { .setRWarning(toString(w$message))	}, \n"
		"	error	= function(e) { .setRError(  toString(e$message))	}  \n"
		"); \n"
		"returnVal");

	SEXP result = jaspRCPP_parseEval(filterTryCatch);

	jaspRCPP_checkForCrashRequest();

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

const char* STDCALL jaspRCPP_saveImage(const char * data, const char *type, const int height, const int width, const int ppi, const char* imageBackground)
{
	RInside &rInside = rinside->instance();

	rInside[".imageBackground"] = imageBackground;
	rInside["plotName"]			= CSTRING_TO_R(data);
	rInside["format"]			= type;
	rInside["height"]			= height;
	rInside["width"]			= width;
	rInside[".ppi"]				= ppi;

	static std::string staticResult;
	staticResult = jaspRCPP_parseEvalStringReturn("jaspBase:::saveImage(plotName, format, height, width)", true);
	return staticResult.c_str();
}

const char* STDCALL jaspRCPP_editImage(const char * name, const char * optionsJson, const int ppi, const char* imageBackground, int analysisID)
{

	RInside &rInside = rinside->instance();

	rInside[".imageBackground"] = imageBackground;
	rInside[".editImgOptions"]	= CSTRING_TO_R(optionsJson);
	rInside[".analysisName"]	= CSTRING_TO_R(name);
	rInside[".ppi"]				= ppi;

	_setJaspResultsInfo(analysisID, 0, false);

	static std::string staticResult;
	staticResult =  jaspRCPP_parseEvalStringReturn("jaspBase:::editImage(.analysisName, .editImgOptions)", true);

	return staticResult.c_str();
}


void STDCALL jaspRCPP_rewriteImages(const char * name, const int ppi, const char* imageBackground, const char* resultsFont, int analysisID)
{

	RInside &rInside = rinside->instance();

	rInside[".ppi"]				= ppi;
	rInside[".imageBackground"] = imageBackground;
	rInside[".analysisName"]	= CSTRING_TO_R(name);
	rInside[".resultsFont"]     = resultsFont;

	_setJaspResultsInfo(analysisID, 0, false);

	jaspRCPP_parseEvalQNT("jaspBase:::rewriteImages(.analysisName, .ppi, .imageBackground)", true);
}

const char*	STDCALL jaspRCPP_evalRCode(const char *rCode, bool setWd) {
	// Function to evaluate arbitrary R code from C++
	// Returns string if R result is a string, else returns "null"
	// Can also load the entire dataset if need be


	jaspRCPP_logString(std::string("jaspRCPP_evalRCode runs: \n\"") + rCode + "\"\n" );

	lastErrorMessage = "";
	rinside->instance()[".rCode"] = CSTRING_TO_R(rCode);
	const std::string rCodeTryCatch(""
		"returnVal = 'null';	"
		"tryCatch("
		"    suppressWarnings({	returnVal <- eval(parse(text=.rCode))     }),	"
		"    error	= function(e) { .setRError(  paste0(toString(e$message), '\n', paste0(sys.calls(), collapse='\n'))) } 	"
		")"
		"; returnVal	");

	static std::string staticResult;
	try
	{
		staticResult = jaspRCPP_parseEvalStringReturn(rCodeTryCatch, setWd);
	}
	catch(...)
	{
		staticResult = NullString;
	}

	return staticResult.c_str();
}

std::stringstream __cmderLogStream;

int __cmderLogFlush() { __cmderLogStream.flush(); return 0; }

size_t __cmderLogWrite(const void * buf, size_t len)
{
	try {	if(len > 0)	__cmderLogStream.write(static_cast<const char *>(buf), len);	}
	catch (...) {		__cmderLogStream << "Capturing output from R had a problem...\n" << std::flush; }
	return len;
}

///Run Rcode and return all output as if
const char*	STDCALL jaspRCPP_evalRCodeCommander(const char *rCode)
{
	__cmderLogStream.str("");

	logFlushDef originalFlush	= _logFlushFunction;
	_logFlushFunction			= __cmderLogFlush;

	logWriteDef	originalLogger	= _logWriteFunction;
	_logWriteFunction			= __cmderLogWrite;

	lastErrorMessage = "";
	rinside->instance()[".rCode"] = CSTRING_TO_R(rCode);
	const std::string rCodeTryCatch(""
		"withCallingHandlers("															"\n"
		"  { "																			"\n"
		"    options(warn=1);"															"\n"
		"    valVis <- withVisible(eval(parse(text=.rCode)));"							"\n"
		"    if(valVis$visible) print(valVis$value);"									"\n"
		"  },	"																		"\n"
		"  warning = function(w) { cat(paste0('Warning: ', toString(w$message))) },"	"\n"
		"  error   = function(e) { cat(paste0('Error: ',   toString(e$message))) }"		"\n"
		");"
		);

	jaspRCPP_parseEvalQNT(rCodeTryCatch, true, false);

	_logFlushFunction			= originalFlush;
	_logWriteFunction			= originalLogger;

	static std::string staticLog;

	staticLog = __cmderLogStream.str();

	__cmderLogStream.str("");

	return staticLog.c_str();
}

} // extern "C"

SEXP jaspRCPP_requestTempFileNameSEXP(SEXP extension)
{
	const char *root, *relativePath;
	std::string extensionAsString = jaspNativeToUtf8(Rcpp::as<Rcpp::String>(extension));

	if (!requestTempFileNameCB(extensionAsString.c_str(), &root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"]			= CSTRING_TO_R_UTF8(root);
	paths["relativePath"]	= CSTRING_TO_R_UTF8(relativePath);

	return paths;
}

SEXP jaspRCPP_requestSpecificFileNameSEXP(SEXP filename)
{
	const char *root, *relativePath;
	std::string filenameAsString = jaspNativeToUtf8(Rcpp::as<Rcpp::String>(filename));

	if (!requestSpecificFileNameCB(filenameAsString.c_str(), &root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"]			= CSTRING_TO_R_UTF8(root);
	paths["relativePath"]	= CSTRING_TO_R_UTF8(relativePath);

	return paths;
}

SEXP jaspRCPP_requestTempRootNameSEXP()
{
	const char* root = requestTempRootNameCB();

	Rcpp::List paths;
	paths["root"] = CSTRING_TO_R_UTF8(root);
	return paths;
}

SEXP jaspRCPP_allColumnNamesDataset()
{
	size_t			cols;
	const char **	names = getAllColumnNames(cols, true);
	
	Rcpp::StringVector colNames;
	
	for(size_t i=0; i<cols; i++)
		colNames.push_back(names[i]);
	
	return colNames;
}


bool jaspRCPP_requestJaspResultsRelativeFilePath(std::string & root, std:: string & relativePath)
{
	root		 = "";
	relativePath = "";

	const char	* _root,
				* _relativePath;

	if (!requestJaspResultsFileSourceCB(&_root, &_relativePath))
		return false;

	root			= _root;
	relativePath	= _relativePath;

	return true;
}

bool jaspRCPP_requestSpecificRelativeFilePath(std::string specificFilename, std::string & root, std:: string & relativePath)
{
	root		 = "";
	relativePath = "";

	const char	* _root,
				* _relativePath;

	if (!requestSpecificFileNameCB(specificFilename.c_str(), &_root, &_relativePath))
		return false;

	root			= _root;
	relativePath	= _relativePath;

	return true;
}

SEXP jaspRCPP_requestStateFileNameSEXP()
{
	const char* root;
	const char* relativePath;

	if (!requestStateFileSourceCB(&root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"]			= CSTRING_TO_R_UTF8(root);
	paths["relativePath"]	= CSTRING_TO_R_UTF8(relativePath);

	return paths;
}


SEXP jaspRCPP_callbackSEXP(SEXP in, SEXP progress)
{
	std::string inStr	= Rf_isNull(in)			? "null"	: jaspNativeToUtf8(Rcpp::as<Rcpp::String>(in));
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
	jaspRCPP_logString("A message from R: " + static_cast<std::string>(Rcpp::as<Rcpp::String>(Message)) + "\n");
}

void jaspRCPP_setRWarning(SEXP Message)
{
	lastErrorMessage = "Warning: " + jaspNativeToUtf8(Rcpp::as<Rcpp::String>(Message));
}

void jaspRCPP_setRError(SEXP Message)
{
	lastErrorMessage = "Error: " + jaspNativeToUtf8(Rcpp::as<Rcpp::String>(Message));
}

void jaspRCPP_setLog(SEXP Message)
{
	lastErrorMessage = jaspNativeToUtf8(Rcpp::as<Rcpp::String>(Message));
}

int jaspRCPP_dataSetRowCount()
{
	return dataSetRowCount();
}

columnType jaspRCPP_getColumnType(std::string columnName)
{
	return columnType(dataSetGetColumnType(columnName.c_str())); // columnName decoded in rbridge
}

bool jaspRCPP_columnIsScale(		std::string columnName) { return jaspRCPP_getColumnType(columnName) == columnType::scale;		}
bool jaspRCPP_columnIsOrdinal(		std::string columnName) { return jaspRCPP_getColumnType(columnName) == columnType::ordinal;		}
bool jaspRCPP_columnIsNominal(		std::string columnName) { return jaspRCPP_getColumnType(columnName) == columnType::nominal;		}
bool jaspRCPP_columnIsNominalText(	std::string columnName) { return jaspRCPP_getColumnType(columnName) == columnType::nominalText;	}

bool jaspRCPP_setColumnDataAsScale(std::string columnName, Rcpp::RObject scalarData)
{
	//columnName = decodeColumnName(columnName.c_str()); // decoded in rbridge

	if(Rcpp::is<Rcpp::Vector<REALSXP>>(scalarData))
		return _jaspRCPP_setColumnDataAsScale(columnName, Rcpp::as<Rcpp::Vector<REALSXP>>(scalarData));

	static Rcpp::Function asNumeric("as.numeric");

	try
	{
		return _jaspRCPP_setColumnDataAsScale(columnName, Rcpp::as<Rcpp::Vector<REALSXP>>(asNumeric(scalarData)));
	}
	catch(...)
	{
		Rf_error("Something went wrong with the conversion to scalar..");
	}
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
	//columnName = decodeColumnName(columnName.c_str()); // decoded in rbridge

	if(Rcpp::is<Rcpp::Vector<INTSXP>>(ordinalData))
		return _jaspRCPP_setColumnDataAsOrdinal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(ordinalData));

	static Rcpp::Function asFactor("as.factor");

	try
	{
		return _jaspRCPP_setColumnDataAsOrdinal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(asFactor(ordinalData)));
	}
	catch(...)
	{
		Rf_error("Something went wrong with the conversion to ordinal..");
	}
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
	//columnName = decodeColumnName(columnName.c_str()); // decoded in rbridge

	if(Rcpp::is<Rcpp::Vector<INTSXP>>(nominalData))
		return _jaspRCPP_setColumnDataAsNominal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(nominalData));

	static Rcpp::Function asFactor("as.factor");

	try
	{
		return _jaspRCPP_setColumnDataAsNominal(columnName, Rcpp::as<Rcpp::Vector<INTSXP>>(asFactor(nominalData)));
	}
	catch(...)
	{
		Rf_error("Something went wrong with the conversion to nominal..");
	}
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
	//columnName = decodeColumnName(columnName.c_str()); // decoded in rbridge

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
	std::map<std::string, columnType>	columnsRequested;
	std::map<std::string, size_t>		columnsOrder;		//Let's remember the order in which they are requested

	if (Rf_isLogical(allColumns) && Rcpp::as<bool>(allColumns))
	{
		char** columns = readDataColumnNamesCB(colMax);

		if (columns)
		{
			for (size_t i = 0; i < *colMax; i++)
				columnsRequested[columns[i]] = columnType::unknown;
		}
	}

	*colMax = 0;

	auto setTypeRequested = [&] (SEXP cols, columnType SetThis)
	{
		if(Rf_isString(cols))
		{
			std::vector<std::string> tmps = Rcpp::as<std::vector<std::string>>(cols);
			for (const std::string & tmp : tmps)
				if(columnsOrder.count(tmp) == 0)
				{
					columnsRequested[tmp]	= SetThis;
					columnsOrder[tmp]		= (*colMax)++;
				}

				else if(columnsOrder.count(tmp) > 0 && columnsRequested[tmp] == columnType::unknown)
					columnsRequested[tmp] = SetThis; //If type is unknown then we simply overwrite it with a manually specified type of analysis

				else if( !(columnsOrder.count(tmp) > 0 && columnsRequested[tmp] == SetThis) ) //Only give an error if the type is different from what is requested
					Rf_error(("You've specified column '" + tmp + "' for more than one columntype!!!\nNo clue which one we should give back...").c_str());

		}
	};

	setTypeRequested(columns,			columnType::unknown);
	setTypeRequested(columnsAsNumeric,	columnType::scale);
	setTypeRequested(columnsAsOrdinal,	columnType::ordinal);
	setTypeRequested(columnsAsNominal,	columnType::nominal);

	size_t lastOrderedIndex = *colMax;

	*colMax = columnsRequested.size();

	RBridgeColumnType* result = static_cast<RBridgeColumnType*>(calloc(*colMax, sizeof(RBridgeColumnType)));

	for (auto const &columnRequested : columnsRequested)
	{
		size_t colNo		= columnsOrder.count(columnRequested.first) > 0 ? columnsOrder[columnRequested.first] : lastOrderedIndex++;
		result[colNo].name	= strdup(columnRequested.first.c_str());
		result[colNo].type	= int(columnRequested.second);
	}

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
	size_t			colMax		= 0;
	RBridgeColumn * colResults	= readFullDataSetCB(&colMax);
	
	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}


Rcpp::DataFrame jaspRCPP_readFullFilteredDataSet()
{
	size_t			colMax		= 0;
	RBridgeColumn * colResults	= readFullFilteredDataSetCB(&colMax);
	
	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_readFilterDataSet()
{
	size_t			colMax		= 0;
	RBridgeColumn * colResults	= readFilterDataSetCB(&colMax);

	if(colMax == 0)
		return Rcpp::DataFrame();

	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_readDataSetSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	size_t				colMax				= 0;
	RBridgeColumnType * columnsRequested	= jaspRCPP_marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns, &colMax);
	RBridgeColumn	  * colResults			= readDataSetCB(columnsRequested, colMax, true);
	
	freeRBridgeColumnType(columnsRequested, colMax);

	return jaspRCPP_convertRBridgeColumns_to_DataFrame(colResults, colMax);
}

Rcpp::DataFrame jaspRCPP_convertRBridgeColumns_to_DataFrame(const RBridgeColumn* colResults, size_t colMax)
{
	Rcpp::DataFrame dataFrame = Rcpp::DataFrame();

	if (colResults)
	{
		Rcpp::List			list(colMax);
		Rcpp::StringVector	columnNames(colMax);

		for (int i = 0; i < int(colMax); i++)
		{
			const RBridgeColumn &	colResult	= colResults[i];

			columnNames[i] = CSTRING_TO_R_CHARSXP(colResult.name);

			if (colResult.isScale)			list[i] = Rcpp::NumericVector(colResult.doubles, colResult.doubles + colResult.nbRows);
			else if(!colResult.hasLabels)	list[i] = Rcpp::IntegerVector(colResult.ints, colResult.ints + colResult.nbRows);
			else							list[i] = jaspRCPP_makeFactor(Rcpp::IntegerVector(colResult.ints, colResult.ints + colResult.nbRows), colResult.labels, colResult.nbLabels, colResult.isOrdinal);

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

			columnNames[i] = CSTRING_TO_R_CHARSXP(colDescription.name);

			if (colDescription.isScale)				list(i) = Rcpp::NumericVector(0);
			else if (!colDescription.hasLabels)		list(i) = Rcpp::IntegerVector(0);
			else									list(i) = jaspRCPP_makeFactor(Rcpp::IntegerVector(0), colDescription.labels, colDescription.nbLabels, colDescription.isOrdinal);
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
		labels[i] = CSTRING_TO_R_CHARSXP(levels[i]);

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

void jaspRCPP_crashPlease() { shouldCrashSoon = true; }
void jaspRCPP_checkForCrashRequest()
{
	if(shouldCrashSoon)
		throw std::runtime_error("User requested a crash");
}

struct jaspRCPP_Connection
{
	static Rboolean	open(struct Rconn *)		{ return Rboolean::TRUE;	}
	static void		close(struct Rconn *)		{}
	static void		destroy(struct Rconn *)		{}
	static int		fflush(struct Rconn *)		{ return 0;	}

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

void jaspRCPP_parseEvalPreface(const std::string & code, const char * msg = "Evaluating R-code:\n")
{
	jaspRCPP_logString(msg);
	jaspRCPP_Connection::write(code.c_str(), 0, code.size(), nullptr);
	jaspRCPP_logString("\nOutput:\n");
}

std::string __sinkMe(const std::string code)
{
	return	"sink(.outputSink);\n\n" + code; //default type = c('message', 'output') anyway
}

void jaspRCPP_setWorkingDirectory()
{
	std::string root = requestTempRootNameCB();
	std::string code = "setwd(\"" + root + "\")";
	rinside->parseEvalQNT(__sinkMe(code));
}

void jaspRCPP_parseEvalQNT(const std::string & code, bool setWd, bool preface)
{
	if(setWd)
		jaspRCPP_setWorkingDirectory();

	if(preface)	
		jaspRCPP_parseEvalPreface(code);
	
	rinside->parseEvalQNT(__sinkMe(code));
	jaspRCPP_logString("\n");

	rinside->parseEvalQNT("sink();"); //Back to normal!
}

std::string jaspRCPP_parseEvalStringReturn(const std::string & code, bool setWd, bool preface)
{
	RInside::Proxy res = jaspRCPP_parseEval(code, setWd, preface);
	
	return Rf_isString(res) ? jaspNativeToUtf8(Rcpp::as<Rcpp::String>(res)) : NullString;
}


RInside::Proxy jaspRCPP_parseEval(const std::string & code, bool setWd, bool preface)
{
	if (setWd)
		jaspRCPP_setWorkingDirectory();

	if(preface)	
		jaspRCPP_parseEvalPreface(code);
	
	RInside::Proxy returnthis = rinside->parseEvalNT(__sinkMe(code)); //Not throwing is nice actually!
	jaspRCPP_logString("\n");

	rinside->parseEvalQNT("sink();"); //back to normal!
	
	return returnthis;
}

std::string _jaspRCPP_System(std::string cmd)
{
	return _systemFunc(cmd.c_str());
}

///This function runs *code* in a separate instance of R, because the output of install.packages (and perhaps other functions) cannot be captured through the outputsink...
SEXP jaspRCPP_RunSeparateR(SEXP code)
{
	auto bendSlashes = [](std::string input)
	{
#ifdef WIN32
		std::stringstream output;

		for(char k : input)
			if(k == '/')	output << "\\";
			else			output << k;
		return output.str();
#else
		return input;
#endif
	};

	static std::string R = bendSlashes("\""+ _R_HOME + "/bin/R\"");

	std::string codestr = jaspNativeToUtf8(Rcpp::as<Rcpp::String>((code))),
				command = R + " --slave -e \"" + codestr + "\"";

	std::string out = _jaspRCPP_System(command);

	jaspRCPP_logString(out + "\n");

	return Rcpp::wrap(out);
}

// see https://gcc.gnu.org/onlinedocs/gcc-4.8.5/cpp/Stringification.html
#define xstr(s) str(s)
#define str(s)  #s

void jaspRCPP_postProcessLocalPackageInstall(SEXP moduleLibrary)
{
	if(Rf_isString(moduleLibrary))
		_libraryFixerFunc(jaspNativeToUtf8(Rcpp::as<Rcpp::String>(moduleLibrary)).c_str());
	else
		Rf_error("jaspRCPP_postProcessLocalPackageInstall did not receive a string, it should get that and the string should represent some kind of R library path.");
}

Rcpp::String jaspRCPP_encodeColumnName(const Rcpp::String & in)
{
	return CSTRING_TO_R_UTF8(encodeColumnName(jaspRCPP_nativeToUtf8(in).c_str()));
}

Rcpp::String jaspRCPP_decodeColumnName(const Rcpp::String & in)
{
	return CSTRING_TO_R_UTF8(decodeColumnName(jaspRCPP_nativeToUtf8(in).c_str()));
}

Rcpp::String jaspRCPP_encodeAllColumnNames(const Rcpp::String & in)
{
	return CSTRING_TO_R_UTF8(encodeAllColumnNames(jaspRCPP_nativeToUtf8(in).c_str()));
}

Rcpp::String jaspRCPP_decodeAllColumnNames(const Rcpp::String & in)
{
	return CSTRING_TO_R_UTF8(decodeAllColumnNames(jaspRCPP_nativeToUtf8(in).c_str()));
}


///Makes sure that whatever string encoding it got, it comes out as utf-8. At least when it is ran *inside* jasp(engine)
std::string jaspRCPP_nativeToUtf8(const Rcpp::String & string)
{
#ifdef JASP_R_INTERFACE_LIBRARY
	if(string.get_encoding() == CE_UTF8)
		return string;
	
	return _stringNativeToUtf8(string.get_cstring());	
#else
	return string;
#endif
}

// ------------------- Below here be dragons -------------------- //

extern "C" {
//We need to do the following crazy defines to make sure the header actually gets accepted by the compiler...
#define class _class
#define private _private;
#include "R_ext/Connections.h"
}

SEXP jaspRCPP_CreateCaptureConnection()
{
	Rconnection con;

	SEXP rc = PROTECT(R_new_custom_connection("jaspRCPP_OUT", "w", "jaspRCPP_OUT", &con));

	con->incomplete		= FALSE;
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

