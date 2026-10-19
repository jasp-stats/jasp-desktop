//
// Copyright (C) 2013-2026 University of Amsterdam
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
#include <Rinternals.h>
#ifndef JASP_NO_RINSIDE
#include "RInside.h"
#endif

static const	std::string		NullString			= "null";
static			std::string		lastErrorMessage	= "";

ReadDataSetCB					readDataSetCB;
ReadADataSetFilterCB			readDataSetRequestedCB;
RunCallbackCB					runCallbackCB;
ReadADataSetCB					readFullDataSetCB,
								readFullFilteredDataSetCB,
								readFilterDataSetCB,
								readCompColDataSetCB;
ReadDataColumnNamesCB			readDataColumnNamesCB;
RequestTempFileNameCB			requestTempFileNameCB,
								requestSpecificFileNameCB;
RequestTempRootNameCB			requestTempRootNameCB;
ReadDataSetDescriptionCB		readDataSetDescriptionCB;
RequestPredefinedFileSourceCB	requestStateFileSourceCB,
								requestJaspResultsFileSourceCB;

DataSetRowCount					dataSetRowCount;
CreateColumn					dataSetCreateColumn;
DeleteColumn					dataSetDeleteColumn;
GetColumnType					dataSetGetColumnType;
SetColumnDataAndType			dataSetColumnDataAndType;
SetDataSet						dataSetSetDataSet;
GetColumnAnalysisId				dataSetGetColumnAnalysisId,
								dataSetGetColumnOriginalIndex;

EnDecodeDef						encodeColumnName,
								decodeColumnName,
								encodeAllColumnNames,
								decodeAllColumnNames;
DecodeTypeDef					decodeColumnType;

ShouldEnDecodeDef				shouldEncodeColumnName,
								shouldDecodeColumnName;

getColNames						getAllColumnNames;
RequestStringRBridge			computedColumnFilterCB;

static logFlushDef				_logFlushFunction		= nullptr;
static logWriteDef				_logWriteFunction		= nullptr;
static sendFuncDef				_sendToDesktop			= nullptr;
static pollMessagesFuncDef		_pollMessagesFunction	= nullptr;
static systemDef				_systemFunc				= nullptr;
static libraryFixerDef			_libraryFixerFunc		= nullptr;
static std::string				_R_HOME = "";
static bool						_insideJASP				= true;

bool shouldCrashSoon = false; //Simply here to allow a developer to force a crash

//Ugly hack to work around windows messing up environment variables when local+codepage+utf8
//Might not be necessary anymore due to the active codepage being set to utf8 now
extern char * R_TempDir;

// Code from RInside.cpp
int __parseEval(const std::string & line, SEXP & ans)
{
//#ifdef PRINT_ENGINE_MESSAGES
	//jaspRCPP_logString("parseEval: " + line + "\n");
//#endif
	ans = R_NilValue;
	ParseStatus status;
	SEXP cmdSexp, cmdexpr = R_NilValue;
	int i, errorOccurred, rc = 0;

	PROTECT(cmdSexp = Rf_allocVector(STRSXP, 1));
	SET_STRING_ELT(cmdSexp, 0, Rf_mkChar(line.c_str()));

	cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
	if (status == PARSE_OK)
	{
		// Loop is needed here as EXPSEXP might be of length > 1
		for(i = 0; i < Rf_length(cmdexpr); i++){
			ans = R_tryEval(VECTOR_ELT(cmdexpr, i),  Rcpp::Environment::global_env(), &errorOccurred);
			if (errorOccurred) {
				rc = 1;
				break;
			}
		}
	}
	else
		rc = 1;

	UNPROTECT(2);
	return rc;
}

SEXP _parseEval(const std::string &line)
{
	SEXP ans = R_NilValue;
	int rc = __parseEval(line, ans);
	if (rc != 0) {
		throw std::runtime_error(std::string("Error evaluating: ") + line);
	}
	return ans;
}

void _parseEvalQNT(const std::string &line)
{
	try {
		SEXP ans;
		__parseEval(line, ans);
	} catch (...) {
	}
}

extern "C" {
void STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks* callbacks,
	sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction,
	logFlushDef logFlushFunction, logWriteDef logWriteFunction, systemDef systemFunc,
	libraryFixerDef libraryFixerFunc, const char* resultFont, const char * tempDir, const char * initFriendlyFunctionsRCode, bool insideJasp)
{
	_insideJASP = insideJasp;

	_logFlushFunction		= logFlushFunction;
	_logWriteFunction		= logWriteFunction;
	_sendToDesktop			= sendToDesktopFunction;
	_pollMessagesFunction	= pollMessagesFunction;
	_systemFunc				= systemFunc;
	_libraryFixerFunc		= libraryFixerFunc;

#ifndef JASP_NO_RINSIDE
	if (_insideJASP)
		new RInside();
#endif

	auto rEnvironment = Rcpp::Environment::global_env();
	R_TempDir = (char*)tempDir;


	requestJaspResultsFileSourceCB				= callbacks->requestJaspResultsFileSourceCB;
	dataSetGetColumnOriginalIndex				= callbacks->dataSetGetColumnOriginalIndex;
	dataSetGetColumnAnalysisId					= callbacks->dataSetGetColumnAnalysisId;
	dataSetColumnDataAndType					= callbacks->dataSetColumnAsDataAndType;
	dataSetSetDataSet							= callbacks->dataSetSetDataSet;
	requestSpecificFileNameCB					= callbacks->requestSpecificFileNameCB;
	readFullFilteredDataSetCB					= callbacks->readFullFilteredDataSetCB;
	requestStateFileSourceCB					= callbacks->requestStateFileSourceCB;
	readDataSetDescriptionCB					= callbacks->readDataSetDescriptionCB;
	readDataSetRequestedCB						= callbacks->readDataSetRequestedCB;
	computedColumnFilterCB						= callbacks->computedColumnFilter;
	requestTempRootNameCB						= callbacks->requestTempRootNameCB;
	requestTempFileNameCB						= callbacks->requestTempFileNameCB;
	readDataColumnNamesCB						= callbacks->readDataColumnNamesCB;
	dataSetGetColumnType						= callbacks->dataSetGetColumnType;
	readCompColDataSetCB						= callbacks->readCompColDataSetCB;
	readFilterDataSetCB							= callbacks->readFilterDataSetCB;
	dataSetCreateColumn							= callbacks->dataSetCreateColumn;
	dataSetDeleteColumn							= callbacks->dataSetDeleteColumn;
	readFullDataSetCB							= callbacks->readFullDataSetCB;
	dataSetRowCount								= callbacks->dataSetRowCount;
	runCallbackCB								= callbacks->runCallbackCB;
	readDataSetCB								= callbacks->readDataSetCB;
	getAllColumnNames							= callbacks->columnNames;
	shouldEncodeColumnName						= callbacks->shouldEncode;
	shouldDecodeColumnName						= callbacks->shouldDecode;
	encodeAllColumnNames						= callbacks->encoderAll;
	decodeAllColumnNames						= callbacks->decoderAll;
	decodeColumnType							= callbacks->decodeType;
	encodeColumnName							= callbacks->encoder;
	decodeColumnName							= callbacks->decoder;

	// TODO: none of this should pollute the global environment.
	rEnvironment[".setLog"]							= Rcpp::InternalFunction(&jaspRCPP_setLog);
	rEnvironment[".setRError"]						= Rcpp::InternalFunction(&jaspRCPP_setRError);
	rEnvironment[".crashPlease"]					= Rcpp::InternalFunction(&jaspRCPP_crashPlease);
	rEnvironment[".setRWarning"]					= Rcpp::InternalFunction(&jaspRCPP_setRWarning);
	rEnvironment[".runSeparateR"]					= Rcpp::InternalFunction(&jaspRCPP_RunSeparateR);
	rEnvironment[".returnString"]					= Rcpp::InternalFunction(&jaspRCPP_returnString);
	rEnvironment[".columnIsScale"]					= Rcpp::InternalFunction(&jaspRCPP_columnIsScale);
	rEnvironment[".callbackNative"]					= Rcpp::InternalFunction(&jaspRCPP_callbackSEXP);
	rEnvironment[".decodeColTypes"]					= Rcpp::InternalFunction(&jaspRCPP_decodeColumnTypeRcpp);
	rEnvironment[".dataSetRowCount"]				= Rcpp::InternalFunction(&jaspRCPP_dataSetRowCount);
	rEnvironment[".returnDataFrame"]				= Rcpp::InternalFunction(&jaspRCPP_returnDataFrame);
	rEnvironment[".columnIsOrdinal"]				= Rcpp::InternalFunction(&jaspRCPP_columnIsOrdinal);
	rEnvironment[".columnIsNominal"]				= Rcpp::InternalFunction(&jaspRCPP_columnIsNominal);
	rEnvironment[".encodeColNamesLax"]				= Rcpp::InternalFunction(&jaspRCPP_encodeAllColumnNames);
	rEnvironment[".decodeColNamesLax"]				= Rcpp::InternalFunction(&jaspRCPP_decodeAllColumnNames);
	rEnvironment[".encodeColNamesStrict"]			= Rcpp::InternalFunction(&jaspRCPP_encodeColumnNameRcpp);
	rEnvironment[".decodeColNamesStrict"]			= Rcpp::InternalFunction(&jaspRCPP_decodeColumnNameRcpp);
	rEnvironment[".setColumnDataAsScale"]			= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsScale);
	rEnvironment[".setDataSet"]						= Rcpp::InternalFunction(&jaspRCPP_setDataSet);
	rEnvironment[".readFullDatasetToEnd"]			= Rcpp::InternalFunction(&jaspRCPP_readFullDataSet);
	rEnvironment[".allColumnNamesDataset"]			= Rcpp::InternalFunction(&jaspRCPP_allColumnNamesDataset);
	rEnvironment[".readDatasetToEndNative"]			= Rcpp::InternalFunction(&jaspRCPP_readDataSetSEXP);
	rEnvironment[".readFilterDatasetToEnd"]			= Rcpp::InternalFunction(&jaspRCPP_readFilterDataSet);
	rEnvironment[".readCompColDatasetToEnd"]		= Rcpp::InternalFunction(&jaspRCPP_readCompColDataSet);
	rEnvironment[".setColumnDataAsOrdinal"]			= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsOrdinal);
	rEnvironment[".setColumnDataAsNominal"]			= Rcpp::InternalFunction(&jaspRCPP_setColumnDataAsNominal);
	rEnvironment[".readDataSetHeaderNative"]		= Rcpp::InternalFunction(&jaspRCPP_readDataSetHeaderSEXP);
	rEnvironment[".createCaptureConnection"]		= Rcpp::InternalFunction(&jaspRCPP_CreateCaptureConnection);
	rEnvironment[".postProcessLibraryModule"]		= Rcpp::InternalFunction(&jaspRCPP_postProcessLocalPackageInstall);
	rEnvironment[".requestTempFileNameNative"]		= Rcpp::InternalFunction(&jaspRCPP_requestTempFileNameSEXP);
	rEnvironment[".requestTempRootNameNative"]		= Rcpp::InternalFunction(&jaspRCPP_requestTempRootNameSEXP);
	rEnvironment[".readDataSetRequestedNative"]		= Rcpp::InternalFunction(&jaspRCPP_readDataSetRequested);
	rEnvironment[".requestStateFileNameNative"]		= Rcpp::InternalFunction(&jaspRCPP_requestStateFileNameSEXP);
	rEnvironment[".readFullFilteredDatasetToEnd"]	= Rcpp::InternalFunction(&jaspRCPP_readFullFilteredDataSet);
	rEnvironment[".requestSpecificFileNameNative"]	= Rcpp::InternalFunction(&jaspRCPP_requestSpecificFileNameSEXP);

	jaspRCPP_logString("Creating Output sink.\n");
	rEnvironment[".outputSink"]						= jaspRCPP_CreateCaptureConnection();

	_parseEvalQNT("sink(.outputSink); print('.outputSink initialized!'); sink();");
	Rcpp::RObject sinkObj = rEnvironment[".outputSink"];
	//jaspRCPP_logString(sinkObj.isNULL() ? "sink is null\n" : !sinkObj.isObject() ? " sink is not object\n" : sinkObj.isS4() ? "sink is s4\n" : "sink is obj but not s4\n");

	_parseEvalQNT("sink(.outputSink); print(.libPaths()); sink();");
	// initialize everything unrelated to jaspBase
	static const char *baseCitationFormat	= "JASP Team (%s). JASP (Version %s) [Computer software].";
	char baseCitation[200];
	snprintf(baseCitation, 200, baseCitationFormat, buildYear, version);

	rEnvironment[".baseCitation"]	= baseCitation;
	rEnvironment[".jaspVersion"]		= version;

	rEnvironment[".baseCitation"]					= baseCitation;
	rEnvironment[".numDecimals"]					= 3;
	rEnvironment[".fixedDecimals"]					= false;
	rEnvironment[".normalizedNotation"]				= true;
	rEnvironment[".exactPValues"]					= false;
	rEnvironment[".resultFont"]						= "Arial";
	rEnvironment[".imageBackground"]				= "transparent";
	rEnvironment[".ppi"]							= 300;

	jaspRCPP_parseEvalQNT("library(methods)");

	if (initFriendlyFunctionsRCode && initFriendlyFunctionsRCode[0] != '\0')
	{
		jaspRCPP_logString("Loading friendly R functions for computed columns and filters.");
		jaspRCPP_parseEvalQNT(initFriendlyFunctionsRCode, false, false);
	}

	_R_HOME = jaspRCPP_parseEvalStringReturn("R.home('')");
	jaspRCPP_logString("jaspRCPP_init is done, R_HOME is: " + _R_HOME + "\n");

}

void STDCALL jaspRCPP_init_jaspBase()
{
	jaspRCPP_logString("Start initializing jaspBase\n");

	//XPtr doesnt like it if it can't run a finalizer so here are some dummy variables:
	static logFuncDef				_logFuncDef					= jaspRCPP_logString;
	static getColumnTypeFuncDef		_getColumnTypeFuncDef		= jaspRCPP_getColumnType;
	static getColumnExistsFDef		_getColumnExistsFuncDef		= jaspRCPP_getColumnExists;
	static createColumnFuncDef		_createColumnFuncDef		= jaspRCPP_createColumn;
	static deleteColumnFuncDef		_deleteColumnFuncDef		= jaspRCPP_deleteColumn;
	static getColumnAnIdFuncDef		_getColumnAnIdFuncDef		= jaspRCPP_getColumnAnalysisId;
	static getColumnAnIdFuncDef		_getColumnIndexFuncDef		= jaspRCPP_getColumnOriginalIndex;
	static setColumnDataFuncDef		_setColumnDataAsScale		= jaspRCPP_setColumnDataAsScale;
	static setColumnDataFuncDef		_setColumnDataAsOrdinal		= jaspRCPP_setColumnDataAsOrdinal;
	static setColumnDataFuncDef		_setColumnDataAsNominal		= jaspRCPP_setColumnDataAsNominal;
	static shouldEnDecodeFuncDef	_shouldEncodeColumnName		= jaspRCPP_shouldEncodeColumnName;
	static shouldEnDecodeFuncDef	_shouldDecodeColumnName		= jaspRCPP_shouldDecodeColumnName;

	static enDecodeFuncDef			_encodeColumnName			= jaspRCPP_encodeColumnName;
	static enDecodeFuncDef			_decodeColumnName			= jaspRCPP_decodeColumnName;

	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment[".logString"]						= Rcpp::XPtr<logFuncDef>(			& _logFuncDef, false);
	rEnvironment[".createColumn"]					= Rcpp::XPtr<createColumnFuncDef>(	& _createColumnFuncDef, false);
	rEnvironment[".deleteColumn"]					= Rcpp::XPtr<deleteColumnFuncDef>(	& _deleteColumnFuncDef, false);
	rEnvironment[".getColumnType"]					= Rcpp::XPtr<getColumnTypeFuncDef>(	& _getColumnTypeFuncDef, false);
	rEnvironment[".getColumnExists"]				= Rcpp::XPtr<getColumnExistsFDef>(	& _getColumnExistsFuncDef, false);
	rEnvironment[".getColumnAnalysisId"]			= Rcpp::XPtr<getColumnAnIdFuncDef>(	& _getColumnAnIdFuncDef, false);
	rEnvironment[".getColumnOriginalIndex"]			= Rcpp::XPtr<getColumnAnIdFuncDef>(	& _getColumnIndexFuncDef, false);
	rEnvironment[".sendToDesktopFunction"]			= Rcpp::XPtr<sendFuncDef>(			& _sendToDesktop, false);
	rEnvironment[".pollMessagesFunction"]			= Rcpp::XPtr<pollMessagesFuncDef>(	& _pollMessagesFunction, false);
	rEnvironment[".setColumnDataAsScalePtr"]		= Rcpp::XPtr<setColumnDataFuncDef>(	& _setColumnDataAsScale, false);
	rEnvironment[".setColumnDataAsOrdinalPtr"]		= Rcpp::XPtr<setColumnDataFuncDef>(	& _setColumnDataAsOrdinal, false);
	rEnvironment[".setColumnDataAsNominalPtr"]		= Rcpp::XPtr<setColumnDataFuncDef>(	& _setColumnDataAsNominal, false);
	rEnvironment[".shouldEncodeColName"]			= Rcpp::XPtr<shouldEnDecodeFuncDef>(& _shouldEncodeColumnName, false);
	rEnvironment[".shouldDecodeColName"]			= Rcpp::XPtr<shouldEnDecodeFuncDef>(& _shouldDecodeColumnName, false);
	rEnvironment[".encodeColName"]					= Rcpp::XPtr<enDecodeFuncDef>(		& _encodeColumnName, false);
	rEnvironment[".decodeColName"]					= Rcpp::XPtr<enDecodeFuncDef>(		& _decodeColumnName, false);

	//Pass a whole bunch of pointers to jaspBase
	jaspRCPP_parseEvalQNT("jaspBase:::setColumnFuncs(		.setColumnDataAsScalePtr, .setColumnDataAsOrdinalPtr, .setColumnDataAsNominalPtr, .getColumnType, .getColumnAnalysisId, .getColumnOriginalIndex, .createColumn, .deleteColumn, .getColumnExists, .encodeColName, .decodeColName, .shouldEncodeColName, .shouldDecodeColName)");
	jaspRCPP_parseEvalQNT("jaspBase:::setJaspLogFunction(	.logString					)");
	jaspRCPP_parseEvalQNT("jaspBase:::setSendFunc(			.sendToDesktopFunction)");
	jaspRCPP_parseEvalQNT("jaspBase:::setPollMessagesFunc(	.pollMessagesFunction)");
	jaspRCPP_parseEvalQNT("jaspBase:::setBaseCitation(		.baseCitation)");
	if (_insideJASP)
		jaspRCPP_parseEvalQNT("jaspBase:::setInsideJasp()");
	jaspRCPP_parseEvalQNT("jaspBase:::registerFonts()");

	//Load it
	jaspRCPP_logString("Initializing jaspBase.\n");
	jaspRCPP_parseEvalQNT("library(jaspBase)");

	jaspRCPP_logString("initializeDoNotRemoveList().\n");
	jaspRCPP_parseEvalQNT("jaspBase:::.initializeDoNotRemoveList()");

	jaspRCPP_logString("Finished initializing jaspBase.\n");
}

void STDCALL jaspRCPP_purgeGlobalEnvironment()
{
	jaspRCPP_parseEvalQNT("jaspBase:::.cleanEngineMemory()", false);
}

void _setJaspResultsInfo(int analysisID, int analysisRevision, bool developerMode)
{
	jaspRCPP_parseEvalQNT(
		"jaspBase:::setResponseData(" + std::to_string(analysisID) +", " + std::to_string(analysisRevision) + ");\n" +
		"jaspBase:::setDeveloperMode(" + (developerMode ? "TRUE" : "FALSE") + ")"
	);

	std::string root, relativePath;

	if(!jaspRCPP_requestJaspResultsRelativeFilePath(root, relativePath))
		throw std::runtime_error("Did not receive a valid filename to store jaspResults.json at.");

	jaspRCPP_parseEvalQNT("jaspBase:::setSaveLocation(\"" + root + "\", \"" + relativePath + "\");");

	std::string specificFilename = jaspRCPP_parseEvalStringReturn("jaspBase:::writeSealFilename()");
	if(!jaspRCPP_requestSpecificRelativeFilePath(specificFilename, root, relativePath))
			throw std::runtime_error("Did not receive a valid filename to store jaspResults write seal at.");

	jaspRCPP_parseEvalQNT("jaspBase:::setWriteSealLocation(\"" + root + "\", \"" + relativePath + "\");");
}

void STDCALL jaspRCPP_setDecimalSettings(int numDecimals, bool fixedDecimals, bool normalizedNotation, bool exactPValues)
{
	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment[".numDecimals"]		= numDecimals;
	rEnvironment[".fixedDecimals"]		= fixedDecimals;
	rEnvironment[".normalizedNotation"]	= normalizedNotation;
	rEnvironment[".exactPValues"]		= exactPValues;
}

void STDCALL jaspRCPP_setFontAndPlotSettings(const char * resultFont, const int ppi, const char* imageBackground)
{
	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment[".resultFont"]				= resultFont;
	rEnvironment[".imageBackground"]		= imageBackground;
	rEnvironment[".ppi"]					= ppi;

	// sometimes jaspBase is not available, check this using https://stackoverflow.com/a/38082613
	std::string result = jaspRCPP_parseEvalStringReturn("if (nzchar(system.file(package = \"jaspBase\"))) \"ok\" else \"not ok\"");
	if (result == "ok")
		jaspRCPP_parseEvalQNT("jaspBase:::registerFonts()");
	else
		jaspRCPP_logString("jaspBase unavailable, did not call jaspBase:::registerFonts()\n");
}

const char* STDCALL jaspRCPP_runModuleCall(const char* name, const char* title, const char* moduleCall, const char* dataKey, const char* options,
										   const char* stateKey, int analysisID, int analysisRevision, bool developerMode, bool preloadData)
{
	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment["name"]				= name;
	rEnvironment["title"]				= title;
	rEnvironment["options"]				= options;
	rEnvironment["dataKey"]				= dataKey;
	rEnvironment["stateKey"]			= stateKey;
	rEnvironment["moduleCall"]			= moduleCall;
	rEnvironment["preloadData"]			= preloadData;
	rEnvironment["resultsMeta"]			= "null";
	rEnvironment["requiresInit"]		= false;

	_setJaspResultsInfo(analysisID, analysisRevision, developerMode);

	static std::string str;

	try
	{
		SEXP results = jaspRCPP_parseEval("jaspBase::runJaspResults(name=name, title=title, dataKey=dataKey, options=options, stateKey=stateKey, functionCall=moduleCall, preloadData=preloadData)", true);

		if(results != NULL && Rcpp::is<std::string>(results))	str = Rcpp::as<Rcpp::String>(results);
		else													str = "error!";
	}
	catch(std::runtime_error & e)
	{
		rEnvironment["errorJaspResultsCrash"] = e.what();
		jaspRCPP_parseEvalQNT("jaspBase:::sendFatalErrorMessage(name=name, title=title, msg=errorJaspResultsCrash);");
	}

#ifdef PRINT_ENGINE_MESSAGES
	jaspRCPP_logString("result of runJaspResults:\n" + str + "\n");
#endif

	jaspRCPP_parseEvalQNT("jaspBase:::destroyAllAllocatedObjects()");

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

	jaspRCPP_resetErrorMsg();

	auto rEnvironment = Rcpp::Environment::global_env();
	rEnvironment[".filterCode"] = filterCode;

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

const char* STDCALL jaspRCPP_saveImage(const char * data, const char *type, const int height, const int width)
{
	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment["plotName"]			= data;
	rEnvironment["format"]				= type;
	rEnvironment["height"]				= height;
	rEnvironment["width"]				= width;

	static std::string staticResult;
	staticResult = jaspRCPP_parseEvalStringReturn("jaspBase:::saveImage(plotName, format, height, width)", true);
	return staticResult.c_str();
}

const char* STDCALL jaspRCPP_editImage(const char * name, const char * optionsJson, int analysisID)
{

	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment[".editImgOptions"]		= optionsJson;
	rEnvironment[".analysisName"]		= name;

	_setJaspResultsInfo(analysisID, 0, false);

	static std::string staticResult;
	staticResult =  jaspRCPP_parseEvalStringReturn("jaspBase:::editImage(.analysisName, .editImgOptions)", true);

	return staticResult.c_str();
}


void STDCALL jaspRCPP_rewriteImages(const char * name, int analysisID)
{

	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment[".analysisName"]		= name;

	_setJaspResultsInfo(analysisID, 0, false);

	jaspRCPP_parseEvalQNT("jaspBase:::rewriteImages(.analysisName, .ppi, .imageBackground)", true);
}



const char*	STDCALL jaspRCPP_evalComputedColumn(const char *rCode, const char * setColumnCode)
{
	// Function to evaluate computed column R code from C++
	// Returns string if R result is a string, else returns "null"
	// Can also load the entire dataset if need be
	bool setWd = false;

	//jaspRCPP_logString(std::string("jaspRCPP_evalComputedColumn runs: \n\"") + rCode + "\"\nand \""+setColumnCode+"\"\n" );

	lastErrorMessage = "";
	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment[".rCode"] = rCode;
	const std::string rCodeTryCatch(""
		"returnVal = NULL;	"
		"tryCatch("
		"    suppressWarnings({	returnVal <- eval(parse(text=.rCode))     }),	"
		"    error	= function(e) { .setRError( toString(e$message)) } 	"
		")"
		"; returnVal	");

	static std::string staticResult;
	try
	{
		try
		{
			rEnvironment[".calcedVals"]	= Rcpp::RObject(jaspRCPP_parseEval(rCodeTryCatch,	false, false));
		}
		catch(std::runtime_error e)
		{
			jaspRCPP_setErrorMsg(e.what());
			staticResult						=	NullString;
			rEnvironment[".calcedVals"]	=	NULL;
		}

		//Only write results into the column when the user code actually produced values; otherwise the
		//setter would replace good computed data with an empty/NA column on an R error.
		Rcpp::RObject calcedVals = rEnvironment[".calcedVals"];
		if (Rf_isNull(calcedVals))
		{
			if (lastErrorMessage.empty())
				jaspRCPP_setErrorMsg("The computed-column R code produced no results, the column was left unchanged.");
			staticResult = NullString;
		}
		else
			staticResult = jaspRCPP_parseEvalStringReturn(setColumnCode,	false, false);

		rEnvironment[".calcedVals"]	=	NULL;

	}
	catch(...)
	{
		staticResult = NullString;
	}

	return staticResult.c_str();
}

const char*	STDCALL jaspRCPP_evalRCode(const char *rCode, bool setWd) {
	// Function to evaluate arbitrary R code from C++
	// Returns string if R result is a string, else returns "null"
	// Can also load the entire dataset if need be


	jaspRCPP_logString(std::string("jaspRCPP_evalRCode runs: \n\"") + rCode + "\"\n" );

	lastErrorMessage = "";
	auto rEnvironment = Rcpp::Environment::global_env();
	rEnvironment[".rCode"] = rCode;
	const std::string rCodeTryCatch(""
		"returnVal = 'null';	"
		"tryCatch("
		"    suppressWarnings({	returnVal <- eval(parse(text=.rCode)); }),	"
		"    error	= function(e) { .setRError(  paste0(toString(e$message), '\n', paste0(sys.calls()[sys.nframe():2], collapse='\n'))) } 	"
		")"
		"; returnVal	");

	static std::string staticResult;
	try
	{
		staticResult = jaspRCPP_parseEvalStringReturn(rCodeTryCatch, setWd);
	}
	catch(std::exception& e)
	{
		jaspRCPP_logString(std::string("Error when running code ") + rCodeTryCatch + ": " + e.what() + "\n");
		staticResult = NullString;
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

	auto rEnvironment = Rcpp::Environment::global_env();
	rEnvironment[".rCode"] = rCode;
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

	jaspRCPP_parseEvalQNT(rCodeTryCatch, false, false);

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
	std::string extensionAsString = Rcpp::as<std::string>(extension);

	if (!requestTempFileNameCB(extensionAsString.c_str(), &root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"]			= root;
	paths["relativePath"]	= relativePath;

	return paths;
}

SEXP jaspRCPP_requestSpecificFileNameSEXP(SEXP filename)
{
	const char *root, *relativePath;
	std::string filenameAsString = Rcpp::as<std::string>(filename);

	if (!requestSpecificFileNameCB(filenameAsString.c_str(), &root, &relativePath))
		return R_NilValue;

	Rcpp::List paths;
	paths["root"]			= root;
	paths["relativePath"]	= relativePath;

	return paths;
}

SEXP jaspRCPP_requestTempRootNameSEXP()
{
	const char* root = requestTempRootNameCB();

	Rcpp::List paths;
	paths["root"] = root;
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
	jaspRCPP_logString("A message from R: " + Rcpp::as<std::string>(Message) + "\n");
}

void jaspRCPP_setRWarning(SEXP Message)
{
	lastErrorMessage = Rcpp::as<std::string>(Message);
}

void jaspRCPP_setRError(SEXP Message)
{
	lastErrorMessage = Rcpp::as<std::string>(Message);
}

void jaspRCPP_setLog(SEXP Message)
{
	lastErrorMessage = Rcpp::as<std::string>(Message);
}

int jaspRCPP_dataSetRowCount()
{
	return dataSetRowCount();
}

columnType jaspRCPP_getColumnType(std::string columnName)
{
	return columnType(dataSetGetColumnType(columnName.c_str())); // columnName decoded in rbridge
}

int jaspRCPP_getColumnAnalysisId(std::string columnName)
{
	return dataSetGetColumnAnalysisId(columnName.c_str()); // columnName decoded in rbridge
}


int jaspRCPP_getColumnOriginalIndex(std::string columnName)
{
	return dataSetGetColumnOriginalIndex(columnName.c_str()); // columnName decoded in rbridge
}

std::string jaspRCPP_createColumn(std::string columnName, bool computed)
{
	return dataSetCreateColumn(columnName.c_str(), computed);
}

bool jaspRCPP_deleteColumn(std::string columnName)
{
	return dataSetDeleteColumn(columnName.c_str());
}


bool jaspRCPP_getColumnExists(std::string columnName)
{

	size_t			cols;
	const char	**	decoded	= getAllColumnNames(cols, false);

	for(size_t i=0; i<cols; i++)
		if(decoded[i] == columnName)
			return true;

	//Maybe the R author already encoded the name?
	const char	**	encoded = getAllColumnNames(cols, true); // This also changes `decoded` above, and in fact, `decoded == encoded` probably
	for(size_t i=0; i<cols; i++)
		if(encoded[i] == columnName)
			return true;

	return false;
}

bool jaspRCPP_columnIsScale(		const std::string & columnName) { return jaspRCPP_getColumnType(columnName) == columnType::scale;		}
bool jaspRCPP_columnIsOrdinal(		const std::string & columnName) { return jaspRCPP_getColumnType(columnName) == columnType::ordinal;		}
bool jaspRCPP_columnIsNominal(		const std::string & columnName) { return jaspRCPP_getColumnType(columnName) == columnType::nominal;		}

bool jaspRCPP_setColumnDataAsScale(const std::string & columnName, Rcpp::RObject scalarData, int computed)
{
	return _jaspRCPP_setColumnDataAndType(columnName, scalarData, columnType::scale, computed==1);
}


bool jaspRCPP_setColumnDataAsOrdinal(const std::string & columnName, Rcpp::RObject ordinalData, int computed)
{
	return _jaspRCPP_setColumnDataAndType(columnName, ordinalData, columnType::ordinal, computed==1);
}


bool jaspRCPP_setColumnDataAsNominal(const std::string & columnName, Rcpp::RObject nominalData, int computed)
{
	return _jaspRCPP_setColumnDataAndType(columnName, nominalData, columnType::nominal, computed==1);
}

bool _jaspRCPP_setColumnDataAndType(const std::string & columnName, Rcpp::RObject data, columnType colType, bool computed)
{
	static Rcpp::Function asNumeric("as.numeric");
	static Rcpp::Function asCharacter("as.character");

	Rcpp::Vector<STRSXP>	strData = Rf_isNull(data) ? Rcpp::CharacterVector()	: Rcpp::CharacterVector(asCharacter(Rcpp::_["x"] = data));
	Rcpp::Vector<REALSXP>	dblData = Rf_isNull(data) ? Rcpp::NumericVector()	: Rcpp::NumericVector(	asNumeric(	Rcpp::_["x"] = data));

	stringvec convertedStrings(strData.begin(), strData.end());

	const char ** nominals = new const char*[convertedStrings.size()]();

	for(size_t i=0; i<convertedStrings.size(); i++)
	{
		bool	isNA  = convertedStrings[i] == "NA",
				isLgl = convertedStrings[i] == "TRUE" || convertedStrings[i] == "FALSE";

		nominals[i] = std::isnan(dblData[i])	// If the string could not be converted to a number its not TRUE or FALSE, but it might be NA. We do not want that as a result!
					? (!isNA	? convertedStrings[i].c_str() : "")
					: (!isLgl	? convertedStrings[i].c_str() : convertedStrings[i] == "TRUE" ? "1" : "0"); //Also getting TRUE or FALSE is not ideal
	}

	return dataSetColumnDataAndType(columnName.c_str(), nominals, static_cast<size_t>(strData.size()), int(colType), computed);
}

bool jaspRCPP_setDataSet(const std::string & datasetName, Rcpp::RObject dfObj)
{
	//The whole output dataset is replaced by this frame, so we must refuse anything that is not a
	//data.frame (e.g. a matrix or list slips past the NULL/empty-frame guard in the caller). Report
	//the real cause instead of silently writing an empty dataset.
	if(!Rcpp::is<Rcpp::DataFrame>(dfObj))
	{
		std::string what = "The R code produced a non-data.frame result that cannot be used to replace the output dataset";
		jaspRCPP_setErrorMsg(what.c_str());
		return false;
	}

	Rcpp::DataFrame			df(dfObj);
	size_t					colCount	= df.size();
	Rcpp::CharacterVector	dfNames		= df.names();

	static Rcpp::Function	asNumeric	("as.numeric"),
							asCharacter	("as.character"),
							isOrdered	("is.ordered");

	std::vector<std::string>					names;
	std::vector<int>							types;
	std::vector<std::vector<std::string>>		data;
	std::vector<std::vector<const char *>>		innerPtrs;
	std::vector<const char *>					namePtrs;
	std::vector<int>							typePtrs;
	std::vector<const char **>					dataPtrs;
	std::vector<size_t>							lengths;

	names.reserve(colCount);
	types.reserve(colCount);
	data.reserve(colCount);

	for(size_t i=0; i<colCount; i++)
	{
		Rcpp::RObject colObj	= df[i];
		std::string	 colName	= Rcpp::as<std::string>(dfNames[i]);

		columnType colType;
		if(Rf_isFactor(colObj))						colType = Rcpp::as<bool>(isOrdered(colObj)) ? columnType::ordinal	: columnType::nominal;
		else if(Rf_isLogical(colObj))				colType = columnType::nominal;
		else										colType = columnType::scale;

		Rcpp::Vector<STRSXP>	strData = Rcpp::CharacterVector(asCharacter(Rcpp::_["x"] = colObj));
		Rcpp::Vector<REALSXP>	dblData = Rcpp::NumericVector(	asNumeric(	Rcpp::_["x"] = colObj));

		std::vector<std::string> colData(strData.begin(), strData.end());
		//as.character and as.numeric may not agree on length for unusual R objects, so only read
		//dblData within its own bounds.
		size_t common = std::min(colData.size(), size_t(dblData.size()));
		for(size_t r=0; r<common; r++)
			if(std::isnan(dblData[r]))
				colData[r] = "";

		names	.push_back(colName);
		types	.push_back(int(colType));
		data	.push_back(colData);
	}

	for(size_t i=0; i<colCount; i++)
	{
		innerPtrs.emplace_back(data[i].size());
		for(size_t r=0; r<data[i].size(); r++)
			innerPtrs[i][r] = data[i][r].c_str();

		namePtrs.push_back(names[i].c_str());
		typePtrs.push_back(types[i]);
		dataPtrs.push_back(innerPtrs[i].data());
		lengths.push_back(data[i].size());
	}

	return dataSetSetDataSet(datasetName.c_str(), namePtrs.data(), typePtrs.data(), dataPtrs.data(), lengths.data(), colCount);
}

const char*	STDCALL jaspRCPP_evalComputedDataSet(const char *rCode, const char * setDataSetCode)
{
	// Function to evaluate computed-dataset R code from C++; the user code is expected to
	// produce a data.frame, which is then written into the output dataset by setDataSetCode.
	lastErrorMessage = "";
	auto rEnvironment = Rcpp::Environment::global_env();

	rEnvironment[".rCode"] = rCode;
	const std::string rCodeTryCatch(""
		"returnVal = NULL;	"
		"tryCatch("
		"    suppressWarnings({	returnVal <- eval(parse(text=.rCode))     }),	"
		"    error	= function(e) { .setRError( toString(e$message)) } 	"
		")"
		"; returnVal	");

	static std::string staticResult;
	try
	{
		try
		{
			rEnvironment[".jaspResult"]	= Rcpp::RObject(jaspRCPP_parseEval(rCodeTryCatch,	false, false));
		}
		catch(std::runtime_error e)
		{
			jaspRCPP_setErrorMsg(e.what());
			staticResult						=	NullString;
			rEnvironment[".jaspResult"]	=	NULL;
		}

		//Do not run setDataSetCode (which replaces the whole output dataset) when the user code
		//errored or produced nothing: it would wipe previously-good computed data with an empty frame.
		Rcpp::RObject jaspResult = rEnvironment[".jaspResult"];
		//An empty (0-column) data.frame is not NULL but would still wipe the output; treat it as no result.
		bool isEmptyFrame = Rcpp::is<Rcpp::DataFrame>(jaspResult) && Rf_ncols(jaspResult) == 0;
		if (Rf_isNull(jaspResult) || isEmptyFrame)
		{
			if (lastErrorMessage.empty())
				jaspRCPP_setErrorMsg("The computed-dataset R code produced no result (NULL/empty data.frame), the output dataset was left unchanged.");
			staticResult = NullString;
		}
		else
			staticResult = jaspRCPP_parseEvalStringReturn(setDataSetCode,	false, false);

		rEnvironment[".jaspResult"]	=	NULL;
	}
	catch(...)
	{
		staticResult = NullString;
	}

	return staticResult.c_str();
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
				if(tmp != "")
				{
					if(columnsOrder.count(tmp) == 0)
					{
						columnsRequested[tmp]	= SetThis;
						columnsOrder[tmp]		= (*colMax)++;
					}

					else if(columnsOrder.count(tmp) > 0 && columnsRequested[tmp] == columnType::unknown)
						columnsRequested[tmp] = SetThis; //If type is unknown then we simply overwrite it with a manually specified type of analysis

					else if( !(columnsOrder.count(tmp) > 0 && columnsRequested[tmp] == SetThis) ) //Only give an error if the type is different from what is requested
						Rf_error("You've specified column '%s' for more than one columntype!!!\nNo clue which one we should give back...", tmp.c_str());
				}
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

Rcpp::DataFrame jaspRCPP_readCompColDataSet()
{
	size_t			colMax		= 0;
	RBridgeColumn * colResults	= readCompColDataSetCB(&colMax);

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

Rcpp::DataFrame jaspRCPP_readDataSetRequested()
{
	size_t				colMax				= 0;
	RBridgeColumn	  * colResults			= readDataSetRequestedCB(&colMax, true);

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

			columnNames[i] = colResult.name;

			if (colResult.isScale)			list[i] =						Rcpp::NumericVector(colResult.doubles,	colResult.doubles	+ colResult.nbRows);
			else							list[i] = jaspRCPP_makeFactor(	Rcpp::IntegerVector(colResult.ints,		colResult.ints		+ colResult.nbRows), colResult.labels, colResult.nbLabels, colResult.isOrdinal);

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

			columnNames[i] = colDescription.name;

			if (colDescription.isScale)				list(i) = Rcpp::NumericVector(0);
			else									list(i) = jaspRCPP_makeFactor(Rcpp::IntegerVector(0), colDescription.labels, colDescription.nbLabels, colDescription.isOrdinal);
		}

		list.attr("names") = columnNames;
		dataFrame = Rcpp::DataFrame(list);
	}

	return dataFrame;

}

Rcpp::IntegerVector jaspRCPP_makeFactor(Rcpp::IntegerVector v, char** levels, int nbLevels, bool ordinal)
{
//#ifdef JASP_DEBUG
//	std::cout << "jaspRCPP_makeFactor:\n\tlevels:\n\t\tnum: " << nbLevels << "\n\t\tstrs:\n";
//	for(int i=0; i<nbLevels; i++)
//		std::cout << "\t\t\t'" << levels[i] << "'\n";
//	std::cout << "intVec: ";
//
//	for(int i=0; i<v.size(); i++)
//		std::cout << v[i] << (i < v.size() - 1 ? ", " : "" );
//	std::cout << std::endl;
//#endif

	Rcpp::CharacterVector labels(nbLevels);
	for (int i = 0; i < nbLevels; i++)
		labels[i] = levels[i];

	v.attr("levels") = labels;

	std::vector<std::string> rClass;

	if (ordinal) rClass.push_back("ordered");
	rClass.push_back("factor");

	v.attr("class") = rClass;

	if(v.size() == 0)
		return v;


	return v;
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
	return	"sink(.outputSink);\n" + code; //default type = c('message', 'output') anyway
}

class SinkGuard
{
public:
	SinkGuard()
	{
		_parseEvalQNT(__sinkMe());
	}

	~SinkGuard()
	{
		close();
	}

	void close()
	{
		if (!_active)
			return;

		try
		{
			SEXP ignored = R_NilValue;
			int rc = __parseEval("sink();", ignored);
			if (rc != 0)
				jaspRCPP_logString("SinkGuard failed to close the R output sink.\n");
		}
		catch (const std::exception & exception)
		{
			jaspRCPP_logString(std::string("SinkGuard failed to close the R output sink: ") + exception.what() + "\n");
		}
		catch (...)
		{
			jaspRCPP_logString("SinkGuard failed to close the R output sink with an unknown exception.\n");
		}

		_active = false;
	}

private:
	bool _active = true;
};

void jaspRCPP_setWorkingDirectory()
{
	std::string root = requestTempRootNameCB();
	std::string code = "setwd(\"" + root + "\");";
	SinkGuard sinkGuard;
	_parseEvalQNT(code);
}

void jaspRCPP_parseEvalQNT(const std::string & code, bool setWd, bool preface)
{
	if(setWd)
		jaspRCPP_setWorkingDirectory();

	if(preface)
		jaspRCPP_parseEvalPreface(code);

	SinkGuard sinkGuard;
	_parseEvalQNT(code);
	jaspRCPP_logString("\n");
}

std::string jaspRCPP_parseEvalStringReturn(const std::string & code, bool setWd, bool preface)
{
	SEXP res = jaspRCPP_parseEval(code, setWd, preface);

	return Rf_isString(res) ? Rcpp::as<std::string>(res) : NullString;
}


SEXP jaspRCPP_parseEval(const std::string & code, bool setWd, bool preface)
{
	if (setWd)
		jaspRCPP_setWorkingDirectory();

	if(preface)
		jaspRCPP_parseEvalPreface(code);

	SinkGuard sinkGuard;
	SEXP returnthis = PROTECT(_parseEval(code)); // Keep the result alive while resetting the sink below.
	jaspRCPP_logString("\n");
	sinkGuard.close();

	UNPROTECT(1);
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

	std::string codestr = Rcpp::as<std::string>(code),
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
		_libraryFixerFunc(Rcpp::as<std::string>(moduleLibrary).c_str());
	else
		Rf_error("jaspRCPP_postProcessLocalPackageInstall did not receive a string, it should get that and the string should represent some kind of R library path.");
}

Rcpp::String jaspRCPP_encodeColumnNameRcpp(const Rcpp::String & in)
{
	return encodeColumnName(std::string(in).c_str());
}

Rcpp::String jaspRCPP_decodeColumnNameRcpp(const Rcpp::String & in)
{
	return decodeColumnName(std::string(in).c_str());
}


Rcpp::String jaspRCPP_decodeColumnTypeRcpp(const Rcpp::String & in)
{
	return decodeColumnType(std::string(in).c_str());
}

Rcpp::String jaspRCPP_encodeAllColumnNames(const Rcpp::String & in)
{
	return encodeAllColumnNames(std::string(in).c_str());
}

Rcpp::String jaspRCPP_decodeAllColumnNames(const Rcpp::String & in)
{
	return decodeAllColumnNames(std::string(in).c_str());
}

bool jaspRCPP_shouldEncodeColumnName(std::string in)
{
	return shouldEncodeColumnName(in.c_str());
}

bool jaspRCPP_shouldDecodeColumnName(std::string in)
{
	return shouldDecodeColumnName(in.c_str());
}

std::string jaspRCPP_encodeColumnName(std::string in)
{
	return encodeColumnName(in.c_str());
}

std::string jaspRCPP_decodeColumnName(std::string in)
{
	return decodeColumnName(in.c_str());
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
