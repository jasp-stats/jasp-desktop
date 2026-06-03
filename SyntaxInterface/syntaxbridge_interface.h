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

#ifndef SYNTAXBRIDGE_INTERFACE_H
#define SYNTAXBRIDGE_INTERFACE_H

/// This file contains all the functions that need to be accessible from the jaspSyntax package
/// So make sure to only use C types.

#ifdef _WIN32
#if defined(SYNTAX_INTERFACE_LIBRARY)
#  define SYNTAX_INTERFACE __declspec(dllexport)
#else
#  define SYNTAX_INTERFACE __declspec(dllimport)
#endif
#else
#define SYNTAX_INTERFACE
#endif

#ifdef _WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern "C" {

// Keep these exported structs plain C ABI data. Callers must initialize every
// field explicitly, e.g. with {} in C++ or calloc/memset in C.
struct SyntaxBridgeColumn {
	char	*   name;
	char	**  values;
} ;

struct SyntaxBridgeDataSet {
	char				*	name;
	int						rowCount;
	int						columnCount;
	SyntaxBridgeColumn	*	columns;
};


SYNTAX_INTERFACE void				STDCALL syntaxBridgeCleanup();
SYNTAX_INTERFACE void				STDCALL syntaxBridgeShutdown();
SYNTAX_INTERFACE void				STDCALL syntaxBridgeClearQmlState();
SYNTAX_INTERFACE void				STDCALL syntaxBridgeClearDataSetState();
SYNTAX_INTERFACE void				STDCALL syntaxBridgeClearNativeState();
SYNTAX_INTERFACE void				STDCALL syntaxBridgeLoadDataSet(const SyntaxBridgeDataSet* dataset, bool dbInMemory, int threshold, bool orderLabelsByValue);
SYNTAX_INTERFACE void				STDCALL syntaxBridgeLoadDataSetFromJaspFile(const char * filePath, bool dbInMemory);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeLoadDataSetFromJaspFileStatus(const char * filePath, bool dbInMemory);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeLoadQmlAndParseOptions(const char * moduleName, const char* analysisName, const char* qmlFile, const char* options, const char* version, bool preloadData);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeLoadQmlAndParseOptionsStatus(const char * moduleName, const char* analysisName, const char* qmlFile, const char* options, const char* version, bool preloadData);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeAnalysisOptionsFromJaspFile(const char * filePath, int analysisNr);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeAnalysisOptionsFromJaspFileStatus(const char * filePath, int analysisNr);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeGenerateModuleWrappers(const char* name);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeGenerateAnalysisWrapper(const char* modulePath, const char* analysisName);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeParseDescription(const char* modulePath);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeGetVariableNames();
SYNTAX_INTERFACE void				STDCALL syntaxBridgeSetVerbose(bool verbose);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeColumnEncoderContext();
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeDecodeColumnText(const char* valuesJson, const char* encoderContextJson);

} // extern "C"

#endif // SYNTAXBRIDGE_INTERFACE_H
