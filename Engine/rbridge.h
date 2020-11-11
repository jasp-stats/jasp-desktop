//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include <boost/function.hpp>
#include "../Common/dataset.h"
#include "../R-Interface/jasprcpp_interface.h"
#include "r_functionwhitelist.h"

/* The R Bridge provides functions to the R analyses;
 * i.e. functions to read the data set from shared memory
 * Similarly, it provides functions to C++ code for
 * launching analyses written in R.
 * In this way, it functions as the bridge between the C++
 * application, and the R analyses
 */
extern "C" {
	RBridgeColumn*				STDCALL rbridge_readDataSet(RBridgeColumnType* columns, size_t colMax, bool obeyFilter);
	RBridgeColumn*				STDCALL rbridge_readFullDataSet(		size_t * colMax);
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
	bool						STDCALL rbridge_setColumnAsScale		(const char* columnName, double *		scalarData,		size_t length);
	bool						STDCALL rbridge_setColumnAsOrdinal		(const char* columnName, int *			ordinalData,	size_t length,	const char ** levels, size_t numLevels);
	bool						STDCALL rbridge_setColumnAsNominal		(const char* columnName, int *			nominalData,	size_t length,	const char ** levels, size_t numLevels);
	bool						STDCALL rbridge_setColumnAsNominalText	(const char* columnName, const char **	nominalData,	size_t length);
	int							STDCALL rbridge_dataSetRowCount();
	const char *				STDCALL rbridge_encodeColumnName(		const char * in);
	const char *				STDCALL rbridge_decodeColumnName(		const char * in);
	const char *				STDCALL rbridge_encodeAllColumnNames(	const char * in);
	const char *				STDCALL rbridge_decodeAllColumnNames(	const char * in);
	const char **				STDCALL rbridge_allColumnNames(			size_t & numCols);
	const char *				STDCALL rbridge_system(					const char * cmd);
	void						STDCALL rbridge_moduleLibraryFixer(		const char * moduleLibrary);
}

	typedef boost::function<std::string (const std::string &, int progress)> RCallback;

	void rbridge_init(sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction);


	void rbridge_setFileNameSource(			boost::function<void(const std::string &, std::string &, std::string &)> source);
	void rbridge_setSpecificFileNameSource(	boost::function<void(const std::string &, std::string &, std::string &)> source);
	void rbridge_setStateFileSource(		boost::function<void(std::string &, std::string &)> source);
	void rbridge_setJaspResultsFileSource(	boost::function<void(std::string &, std::string &)> source);
	void rbridge_setDataSetSource(			boost::function<DataSet *()> source);
	void rbridge_memoryCleaning();

	std::string rbridge_runModuleCall(const std::string &name, const std::string &title, const std::string &moduleCall, const std::string &dataKey, const std::string &options, const std::string &stateKey, int ppi, int analysisID, int analysisRevision, const std::string &imageBackground, bool developerMode);

	void rbridge_setColumnFunctionSources(			boost::function<int (const std::string &)																		> getTypeSource,
													boost::function<bool(const std::string &, const std::vector<double>&)											> scaleSource,
													boost::function<bool(const std::string &,		std::vector<int>&,			const std::map<int, std::string>&)	> ordinalSource,
													boost::function<bool(const std::string &,		std::vector<int>&,			const std::map<int, std::string>&)	> nominalSource,
													boost::function<bool(const std::string &, const std::vector<std::string>&)										> nominalTextSource);
	void rbridge_setGetDataSetRowCountSource(		boost::function<int()> source);

	std::string rbridge_check();

	void	rbridge_setupRCodeEnvReadData(const std::string & dataname, const std::string & readFunction);
	void	rbridge_setupRCodeEnv(int rowCount, const std::string & dataname = "data");
	void	rbridge_detachRCodeEnv(				const std::string & dataname = "data");

	void freeRBridgeColumns();
	void freeRBridgeColumnDescription(RBridgeColumnDescription* columns, size_t colMax);
	void freeLabels(char** labels, size_t nbLabels);

	std::vector<bool>	rbridge_applyFilter(					const std::string & filterCode, const std::string & generatedFilterCode);
	std::string			rbridge_encodeColumnNamesInScript(		const std::string & filterCode);
	std::string			rbridge_evalRCodeWhiteListed(			const std::string & rCode);
	void				rbridge_setLANG(						const std::string & lang);
#endif // RBRIDGE_H
