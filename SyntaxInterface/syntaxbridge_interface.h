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

#include <stdio.h>

extern "C" {

struct SyntaxBridgeColumn {
	char	*   name	= nullptr;
	char	**  values	= nullptr;
} ;

struct SyntaxBridgeDataSet {
	char				*	name			= nullptr;
	int						rowCount		= 0;
	int						columnCount		= 0;
	SyntaxBridgeColumn	*	columns			= nullptr;
};


SYNTAX_INTERFACE void				STDCALL syntaxBridgeCleanup();
SYNTAX_INTERFACE void				STDCALL syntaxBridgeLoadDataSet(const SyntaxBridgeDataSet* dataset, bool dbInMemory, int threshold, bool orderLabelsByValue);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeLoadQmlAndParseOptions(const char * moduleName, const char* analysisName, const char* qmlFile, const char* options, const char* version, bool preloadData);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeGenerateModuleWrappers(const char* name, bool preloadData);
SYNTAX_INTERFACE const char*		STDCALL syntaxBridgeGenerateAnalysisWrapper(const char* modulePath, const char* qmlFileName, const char* analysisName, const char* analysisTitle, bool preloadData);

} // extern "C"

#endif // SYNTAXBRIDGE_INTERFACE_H
