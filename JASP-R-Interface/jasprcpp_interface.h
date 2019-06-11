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

#include <QtCore/qglobal.h>

//unix use same compiler so no need for dll-hoops to jump through
#ifdef _WIN32
#if defined(JASP_R_INTERFACE_LIBRARY)
#  define RBRIDGE_TO_JASP_INTERFACE Q_DECL_EXPORT
#else
#  define RBRIDGE_TO_JASP_INTERFACE Q_DECL_IMPORT
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
  bool    hasLabels;
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
  bool    hasLabels;
  bool    isOrdinal;
	char**	labels;
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
typedef bool						(STDCALL *RequestSpecificFileSourceCB)	(const char **root, const char **relativePath);
typedef bool						(STDCALL *RequestTempFileNameCB)        (const char* extensionAsString, const char **root, const char **relativePath);
typedef const char*					(STDCALL *RequestTempRootNameCB)        ();
typedef bool						(STDCALL *RunCallbackCB)                (const char* in, int progress, const char** out);
typedef bool						(STDCALL *SetColumnAsScale)             (const char* columnName, double *       scalarData,		size_t length);
typedef bool						(STDCALL *SetColumnAsOrdinal)           (const char* columnName, int *          ordinalData,	size_t length, const char ** levels, size_t numLevels);
typedef bool						(STDCALL *SetColumnAsNominal)           (const char* columnName, int *          nominalData,	size_t length, const char ** levels, size_t numLevels);
typedef bool						(STDCALL *SetColumnAsNominalText)       (const char* columnName, const char **	nominalData,	size_t length);
typedef int							(STDCALL *DataSetRowCount)              ();


struct RBridgeCallBacks {
	ReadDataSetCB				readDataSetCB;
	ReadDataColumnNamesCB		readDataColumnNamesCB;
	ReadDataSetDescriptionCB	readDataSetDescriptionCB;
	RequestSpecificFileSourceCB	requestStateFileSourceCB;
	RequestTempFileNameCB		requestTempFileNameCB;
	RequestTempRootNameCB		requestTempRootNameCB;
	RunCallbackCB				runCallbackCB;
	ReadADataSetCB				readFullDataSetCB;
	ReadADataSetCB				readFilterDataSetCB;
	RequestSpecificFileSourceCB	requestJaspResultsFileSourceCB;
	SetColumnAsScale			dataSetColumnAsScale;
	SetColumnAsOrdinal			dataSetColumnAsOrdinal;
	SetColumnAsNominal			dataSetColumnAsNominal;
	SetColumnAsNominalText		dataSetColumnAsNominalText;
	DataSetRowCount				dataSetRowCount;
};

typedef void	(*sendFuncDef)			(const char *);
typedef bool	(*pollMessagesFuncDef)	();
typedef int		(*logFlushDef)			();
typedef size_t	(*logWriteDef)			(const void * buf, size_t len);

// Calls from rbridge to jaspRCPP
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks *calbacks, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction, logFlushDef logFlushFunction, logWriteDef logWriteFunction);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_run(const char* name, const char* title, const char* rfile, bool requiresInit, const char* dataKey, const char* options, const char* resultsMeta, const char* stateKey, const char* perform, int ppi, int analysisID, int analysisRevision, bool usesJaspResults, const char* imageBackground);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_check();

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_saveImage(const char *name, const char *type, const int height, const int width, const int ppi, const char* imageBackground);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_editImage(const char *name, const char *type, const int height, const int width, const int ppi, const char* imageBackground);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_rewriteImages(const int ppi, const char* imageBackground);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_runModuleCall(const char* name, const char* title, const char* moduleCall, const char* dataKey, const char* options, const char* stateKey, const char* perform, int ppi, int analysisID, int analysisRevision, const char* imageBackground);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_check();

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_evalRCode(const char *rCode);

RBRIDGE_TO_JASP_INTERFACE int			STDCALL jaspRCPP_runFilter(const char * filtercode, bool ** arraypointer); //arraypointer points to a pointer that will contain the resulting list of filter-booleans if jaspRCPP_runFilter returns > 0
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_freeArrayPointer(bool ** arrayPointer);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_runScript(const char * scriptCode);
RBRIDGE_TO_JASP_INTERFACE const char *	STDCALL jaspRCPP_runScriptReturnString(const char * scriptCode);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_getLastErrorMsg();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_resetErrorMsg();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_setErrorMsg(const char* msg);

} // extern "C"

///New exception to give feedback about possibly failing filters and such
class filterException : public std::logic_error
{
public:
	filterException(const std::string & what_arg)	: std::logic_error(what_arg) {}
	filterException(const char * what_arg)			: std::logic_error(what_arg) {}
};

#endif // JASPRCPP_INTERFACE_H
