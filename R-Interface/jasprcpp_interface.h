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

#ifndef JASPRCPP_INTERFACE_H
#define JASPRCPP_INTERFACE_H

/// This file contains all the functions that need to be accessible in Engine
/// So make sure to only use C types.

//unix uses same compiler for both R-Interface and Engine so no need for dll-hoops to jump through
#ifdef _WIN32
#if defined(JASP_R_INTERFACE_LIBRARY)
#  define RBRIDGE_TO_JASP_INTERFACE __declspec(dllexport)
#else
#  define RBRIDGE_TO_JASP_INTERFACE __declspec(dllimport)
#endif
#else
#define RBRIDGE_TO_JASP_INTERFACE
#endif

#ifdef _WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

#include <stdio.h>

extern "C" {

struct RBridgeColumn {
  char*   name;
  bool    isScale;
  bool    isOrdinal;
  double* doubles;
  int*    ints;
  char**  labels;
  size_t  nbRows;
  size_t  nbLabels;
} ;

struct RBridgeColumnDescription {
  int     type;
  char*   name;
  bool    isScale;
  bool    isOrdinal;
  char**  labels;
  size_t  nbLabels;
} ;

struct RBridgeColumnType {
	char*	name;
	int		type;
};

// Callbacks from jaspRCPP to rbridge
typedef RBridgeColumn*				(STDCALL *ReadDataSetCB)                (RBridgeColumnType* columns, size_t colMax, bool obeyFilter);
typedef RBridgeColumn*				(STDCALL *ReadADataSetCB)               (size_t * colMax);
typedef char**						(STDCALL *ReadDataColumnNamesCB)        (size_t * maxCol);
typedef RBridgeColumnDescription*	(STDCALL *ReadDataSetDescriptionCB)     (RBridgeColumnType* columns, size_t colMax);
typedef bool						(STDCALL *RequestPredefinedFileSourceCB)(const char **root, const char **relativePath);
typedef bool						(STDCALL *RequestTempFileNameCB)        (const char* extensionAsString, const char **root, const char **relativePath);
typedef const char*					(STDCALL *RequestTempRootNameCB)        ();
typedef bool						(STDCALL *RunCallbackCB)                (const char* in, int progress, const char** out);
typedef int							(STDCALL *GetColumnType)				(const char* columnName);
typedef int							(STDCALL *GetColumnAnalysisId)			(const char* columnName);
typedef const char *				(STDCALL *CreateColumn)					(const char* columnName);
typedef bool						(STDCALL *DeleteColumn)					(const char* columnName);
typedef bool						(STDCALL *SetColumnDataAndType)			(const char* columnName, const char **	nominalData,	size_t length, int columnTYpe);
typedef int							(STDCALL *DataSetRowCount)              ();
typedef const char *				(STDCALL *EnDecodeDef)					(const char *);
typedef bool						(STDCALL *ShouldEnDecodeDef)			(const char *);
typedef const char *				(STDCALL *systemDef)					(const char *);
typedef void						(STDCALL *libraryFixerDef)				(const char *);
typedef const char **				(STDCALL *getColNames)					(size_t &  names, bool encoded);

struct RBridgeCallBacks {
	ReadDataSetCB					readDataSetCB;
	ReadDataColumnNamesCB			readDataColumnNamesCB;
	ReadDataSetDescriptionCB		readDataSetDescriptionCB;
	RequestPredefinedFileSourceCB	requestStateFileSourceCB;
	RequestTempFileNameCB			requestTempFileNameCB;
	RequestTempFileNameCB			requestSpecificFileNameCB;
	RequestTempRootNameCB			requestTempRootNameCB;
	RunCallbackCB					runCallbackCB;
	ReadADataSetCB					readFullDataSetCB;
	ReadADataSetCB					readFullFilteredDataSetCB;
	ReadADataSetCB					readFilterDataSetCB;
	RequestPredefinedFileSourceCB	requestJaspResultsFileSourceCB;
	GetColumnType					dataSetGetColumnType;
	CreateColumn					dataSetCreateColumn;
	DeleteColumn					dataSetDeleteColumn;
	GetColumnAnalysisId				dataSetGetColumnAnalysisId;
	SetColumnDataAndType			dataSetColumnAsDataAndType;
	DataSetRowCount					dataSetRowCount;
	EnDecodeDef						encoder,
									decoder,
									encoderAll,
									decoderAll;
	ShouldEnDecodeDef				shouldEncode,
									shouldDecode;
	getColNames						columnNames;
};

typedef void			(*sendFuncDef)			(const char *);
typedef bool			(*pollMessagesFuncDef)	();
typedef int				(*logFlushDef)			();
typedef size_t			(*logWriteDef)			(const void * buf, size_t len);


// Calls from rbridge to jaspRCPP
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks *calbacks, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction, logFlushDef logFlushFunction, logWriteDef logWriteFunction, systemDef systemFunc, libraryFixerDef libraryFixerFunc, const char* resultFont, const char * tempDir);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_init_jaspBase();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_setDecimalSettings(int numDecimals, bool fixedDecimals, bool normalizedNotation, bool exactPValues);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_setFontAndPlotSettings(const char * resultFont, const int ppi, const char* imageBackground);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_runModuleCall(const char* name, const char* title, const char* moduleCall, const char* dataKey, const char* options, const char* stateKey, int analysisID, int analysisRevision, bool developerMode);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_saveImage(const char *data, const char *type, const int height, const int width);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_editImage(const char *name, const char *optionsJson, int analysisID);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_rewriteImages(const char * name, int analysisID);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_evalRCode(			const char *rCode, bool setWd);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_evalRCodeCommander(const char *rCode);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_evalComputedColumn(const char *rCode, const char * setColumnCode);

RBRIDGE_TO_JASP_INTERFACE int			STDCALL jaspRCPP_runFilter(const char * filtercode, bool ** arraypointer); //arraypointer points to a pointer that will contain the resulting list of filter-booleans if jaspRCPP_runFilter returns > 0
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_freeArrayPointer(bool ** arrayPointer);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_runScript(const char * scriptCode);
RBRIDGE_TO_JASP_INTERFACE const char *	STDCALL jaspRCPP_runScriptReturnString(const char * scriptCode);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_getLastErrorMsg();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_resetErrorMsg();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_setErrorMsg(const char* msg);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_purgeGlobalEnvironment();

RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_junctionHelper(bool collectNotRestore, const char * modulesFolder, const char * linkFolder, const char * junctionsFilePath);

} // extern "C"

#endif // JASPRCPP_INTERFACE_H
