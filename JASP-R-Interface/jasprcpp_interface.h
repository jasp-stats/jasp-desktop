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
#ifdef __WIN32__
#if defined(JASP_R_INTERFACE_LIBRARY)
#  define RBRIDGE_TO_JASP_INTERFACE Q_DECL_EXPORT
#else
#  define RBRIDGE_TO_JASP_INTERFACE Q_DECL_IMPORT
#endif
#else
#define RBRIDGE_TO_JASP_INTERFACE
#endif

#ifdef __WIN32__
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern "C" {

struct RBridgeColumn {
	char*	name;
	bool	isScale;
	bool	hasLabels;
	bool	isOrdinal;
	double*	doubles;
	int*	ints;
	char**	labels;
	int		nbRows;
	int		nbLabels;
} ;

struct RBridgeColumnDescription {
	int		type;
	char*	name;
	bool	isScale;
	bool	hasLabels;
	bool	isOrdinal;
	char**	labels;
	int		nbLabels;
} ;

struct RBridgeColumnType {
	char*	name;
	int		type;
};

// Callbacks from jaspRCPP to rbridge
typedef RBridgeColumn*				(STDCALL *ReadDataSetCB)				(RBridgeColumnType* columns, int colMax, bool obeyFilter);
typedef RBridgeColumn*				(STDCALL *ReadADataSetCB)				(int * colMax);
typedef char**						(STDCALL *ReadDataColumnNamesCB)		(int *maxCol);
typedef RBridgeColumnDescription*	(STDCALL *ReadDataSetDescriptionCB)		(RBridgeColumnType* columns, int colMax);
typedef bool						(STDCALL *RequestSpecificFileSourceCB)	(const char **root, const char **relativePath);
typedef bool						(STDCALL *RequestTempFileNameCB)		(const char* extensionAsString, const char **root, const char **relativePath);
typedef const char*					(STDCALL *RequestTempRootNameCB)		();
typedef bool						(STDCALL *RunCallbackCB)				(const char* in, int progress, const char** out);
typedef void						(STDCALL *SetColumnAsScale)				(const char* columnName, double *		scalarData,		size_t length);
typedef void						(STDCALL *SetColumnAsOrdinal)			(const char* columnName, int *			ordinalData,	size_t length);
typedef void						(STDCALL *SetColumnAsNominal)			(const char* columnName, int *			nominalData,	size_t length);
typedef void						(STDCALL *SetColumnAsNominalText)		(const char* columnName, const char **	nominalData,	size_t length);


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
};

typedef void (*sendFuncDef)(const char *);
typedef bool (*pollMessagesFuncDef)();

// Calls from rbridge to jaspRCPP
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_init(const char* buildYear, const char* version, RBridgeCallBacks *calbacks, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_run(const char* name, const char* title, const char* rfile, bool requiresInit, const char* dataKey, const char* options, const char* resultsMeta, const char* stateKey, const char* perform, int ppi, int analysisID, int analysisRevision, bool usesJaspResults);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_runModuleCall(const char* name, const char* title, const char* moduleCall, const char* dataKey, const char* options, const char* stateKey, const char* perform, int ppi, int analysisID, int analysisRevision);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_saveImage(const char *name, const char *type, const int height, const int width, const int ppi);
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_editImage(const char *name, const char *type, const int height, const int width, const int ppi);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_check();

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_evalRCode(const char *rCode);

RBRIDGE_TO_JASP_INTERFACE int			STDCALL jaspRCPP_runFilter(const char * filtercode, bool ** arraypointer); //arraypointer points to a pointer that will contain the resulting list of filter-booleans if jaspRCPP_runFilter returns > 0
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_freeArrayPointer(bool ** arrayPointer);
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_runScript(const char * scriptCode);

RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_getLastErrorMsg();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_resetErrorMsg();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_setErrorMsg(const char* msg);

#ifndef __WIN32__
RBRIDGE_TO_JASP_INTERFACE const char*	STDCALL jaspRCPP_getRConsoleOutput();
RBRIDGE_TO_JASP_INTERFACE void			STDCALL jaspRCPP_clearRConsoleOutput();
#endif


} // extern "C"

///New exception to give feedback about possibly failing filters and such
class filterException : public std::logic_error
{
public:
	filterException(const std::string & what_arg)	: std::logic_error(what_arg) {}
	filterException(const char * what_arg)			: std::logic_error(what_arg) {}
};

#endif // JASPRCPP_INTERFACE_H
