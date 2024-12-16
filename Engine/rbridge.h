//
// Copyright (C) 2013-2024 University of Amsterdam
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

#ifndef RBRIDGE_H
#define RBRIDGE_H

#ifdef _WIN32
#undef Realloc
#undef Free
#endif

#include <string>
#include <map>
#include <set>
#include <regex>
#include "jasprcpp_interface.h"
#include "columnencoder.h"

class EngineBase;

/// The R Bridge provides functions to the R analyses;
/// i.e. functions to read the data set from shared memory
/// Similarly, it provides functions to C++ code for launching analyses written in R.
/// In this way, it functions as the bridge between the C++ application, and the R analyses
/// 
/// But why do we need such a bridge?
/// Because on windows, for some time at least, Qt was only buildable in MSVC and R is only buildable with GCC or something that acts like it.
/// Thus we needed to split R-Interface up into a DLL and can't send over std::string etc, because the implementations aren't the same
/// rbridge handles conversions between the two through const char *.
extern "C" {
	RBridgeColumn*				STDCALL rbridge_readDataSet(RBridgeColumnType* columns, size_t colMax, bool obeyFilter);
	RBridgeColumn*				STDCALL rbridge_readFullDataSet(		size_t * colMax);
	RBridgeColumn*				STDCALL rbridge_readDataSetRequested(	size_t * colMax, bool obeyFilter);
	RBridgeColumn*				STDCALL rbridge_readFullFilteredDataSet(size_t * colMax);
	RBridgeColumn*				STDCALL rbridge_readFullDataSetHelper(	size_t * colMax, bool obeyFilter);
	RBridgeColumn*				STDCALL rbridge_readDataSetForFiltering(size_t * colMax);
	char**						STDCALL rbridge_readDataColumnNames(	size_t *colMax);
	RBridgeColumnDescription*	STDCALL rbridge_readDataSetDescription(RBridgeColumnType* columns, size_t colMax);
	bool						STDCALL rbridge_test(char** root);
	bool						STDCALL rbridge_requestStateFileSource(									const char ** root, const char ** relativePath);
	bool						STDCALL rbridge_requestJaspResultsFileSource(							const char ** root, const char ** relativePath);
	bool						STDCALL rbridge_requestTempFileName(	const char * extensionAsString,	const char ** root, const char ** relativePath);
	bool						STDCALL rbridge_requestSpecificFileName(const char * specificFilename,	const char ** root, const char ** relativePath);
	const char*					STDCALL rbridge_requestTempRootName();
	bool						STDCALL rbridge_runCallback(const char* in, int progress, const char** out);
	int							STDCALL rbridge_getColumnType			(const char * columnName);
	int							STDCALL rbridge_getColumnAnalysisId		(const char * columnName);
	const char *				STDCALL rbridge_createColumn			(const char * columnName);
	bool						STDCALL rbridge_deleteColumn			(const char * columnName);
	bool						STDCALL rbridge_setColumnDataAndType	(const char* columnName, const char **	nominalData,	size_t length,	int columnType);
	int							STDCALL rbridge_dataSetRowCount();
	const char *				STDCALL rbridge_encodeColumnName(		const char * in);
	const char *				STDCALL rbridge_decodeColumnName(		const char * in);
	int							STDCALL rbridge_decodeColumnType(		const char * in);
	const char *				STDCALL rbridge_encodeAllColumnNames(	const char * in);
	const char *				STDCALL rbridge_decodeAllColumnNames(	const char * in);
	bool						STDCALL rbridge_shouldEncodeColumnName(	const char * in);
	bool						STDCALL rbridge_shouldDecodeColumnName(	const char * in);
	const char **				STDCALL rbridge_allColumnNames(			size_t & numCols,	bool encoded);
	const char *				STDCALL rbridge_system(					const char * cmd);
	void						STDCALL rbridge_moduleLibraryFixer(		const char * moduleLibrary);
}

	typedef std::function<std::string (const std::string &, int progress)> RCallback;

	void rbridge_setEngine(EngineBase * engine);
	void rbridge_init(EngineBase * engine, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction, ColumnEncoder * encoder, const char * resultFont);
	void rbridge_junctionHelper(bool collectNotRestore, const std::string & modulesFolder, const std::string& linkFolder, const std::string& junctionFilePath);

	void rbridge_memoryCleaning();

	std::string rbridge_runModuleCall(const std::string &name, const std::string &title, const std::string &moduleCall, const std::string &dataKey, const std::string &options, const std::string &stateKey, int analysisID, int analysisRevision, bool developerMode, ColumnEncoder::colsPlusTypes datasetColsTypes, bool preloadData);

	void	rbridge_setupRCodeEnvReadData(const std::string & dataname, const std::string & readFunction);
	void	rbridge_setupRCodeEnv(int rowCount, const std::string & dataname = "data");
	void	rbridge_detachRCodeEnv(				const std::string & dataname = "data");

	void freeRBridgeColumns();
	void freeRBridgeColumnDescription(RBridgeColumnDescription* columns, size_t colMax);
	void freeLabels(char** labels, size_t nbLabels);

	std::vector<bool>	rbridge_applyFilter(					const std::string & filterCode, const std::string & generatedFilterCode);
	std::string			rbridge_encodeColumnNamesInScript(		const std::string & filterCode);
	std::string			rbridge_evalRCodeWhiteListed(			const std::string & rCode, bool setWd);
	std::string			rbridge_evalRComputedColumn(			const std::string & rCode, const std::string & setColumnCode);
	void				rbridge_setLANG(						const std::string & lang);
#endif // RBRIDGE_H
