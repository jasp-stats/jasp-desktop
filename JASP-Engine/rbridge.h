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

#ifdef __WIN32__

#undef Realloc
#undef Free

#endif

#include <string>
#include <map>
#include <unordered_set>
#include <set>
#include <regex>
#include <boost/function.hpp>
#include "../JASP-Common/dataset.h"
#include "../JASP-R-Interface/jasprcpp_interface.h"
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
	RBridgeColumn*				STDCALL rbridge_readFullDataSet(size_t * colMax);
	RBridgeColumn*				STDCALL rbridge_readDataSetForFiltering(size_t * colMax);
	char**						STDCALL rbridge_readDataColumnNames(size_t *colMax);
	RBridgeColumnDescription*	STDCALL rbridge_readDataSetDescription(RBridgeColumnType* columns, size_t colMax);
	bool						STDCALL rbridge_test(char** root);
	bool						STDCALL rbridge_requestStateFileSource(const char **root, const char **relativePath);
	bool						STDCALL rbridge_requestJaspResultsFileSource(const char **root, const char **relativePath);
	bool						STDCALL rbridge_requestTempFileName(const char* extensionAsString, const char **root, const char **relativePath);
	const char*					STDCALL rbridge_requestTempRootName();
	bool						STDCALL rbridge_runCallback(const char* in, int progress, const char** out);
	bool						STDCALL rbridge_setColumnAsScale		(const char* columnName, double *		scalarData,		size_t length);
	bool						STDCALL rbridge_setColumnAsOrdinal		(const char* columnName, int *			ordinalData,	size_t length,	const char ** levels, size_t numLevels);
	bool						STDCALL rbridge_setColumnAsNominal		(const char* columnName, int *			nominalData,	size_t length,	const char ** levels, size_t numLevels);
	bool						STDCALL rbridge_setColumnAsNominalText	(const char* columnName, const char **	nominalData,	size_t length);
	int							STDCALL rbridge_dataSetRowCount();
}

	typedef boost::function<std::string (const std::string &, int progress)> RCallback;

	void rbridge_init(sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction);

	void rbridge_setFileNameSource(			boost::function<void(const std::string &, std::string &, std::string &)> source);
	void rbridge_setStateFileSource(		boost::function<void(std::string &, std::string &)> source);
	void rbridge_setJaspResultsFileSource(	boost::function<void(std::string &, std::string &)> source);
	void rbridge_setDataSetSource(			boost::function<DataSet *()> source);

	std::string rbridge_runModuleCall(const std::string &name, const std::string &title, const std::string &moduleCall, const std::string &dataKey, const std::string &options, const std::string &stateKey, const std::string &perform, int ppi, int analysisID, int analysisRevision, const std::string &imageBackground);

	void rbridge_setColumnDataAsScaleSource(		boost::function<bool(std::string&, std::vector<double>&)>							source);
	void rbridge_setColumnDataAsOrdinalSource(		boost::function<bool(std::string&, std::vector<int>&, std::map<int, std::string>&)>	source);
	void rbridge_setColumnDataAsNominalSource(		boost::function<bool(std::string&, std::vector<int>&, std::map<int, std::string>&)>	source);
	void rbridge_setColumnDataAsNominalTextSource(	boost::function<bool(std::string&, std::vector<std::string>&)>						source);
	void rbridge_setGetDataSetRowCountSource(		boost::function<int()> source);

	std::string rbridge_run(const std::string &name, const std::string &title, const std::string &rfile, bool &requiresInit, const std::string &dataKey, const std::string &options, const std::string &resultsMeta, const std::string &stateKey, int analysisID, int analysisRevision, const std::string &perform = "run", int ppi = 96, const std::string &imageBackground = "white", RCallback callback = NULL, bool useJaspResults = false);
	std::string rbridge_check();

	void freeRBridgeColumns();
	void freeRBridgeColumnDescription(RBridgeColumnDescription* columns, size_t colMax);
	void freeLabels(char** labels, size_t nbLabels);

	std::vector<bool>	rbridge_applyFilter(std::string & filterCode, std::string & generatedFilterCode);
	std::string			rbridge_encodeColumnNamesToBase64(std::string & filterCode);
	std::string			rbridge_decodeColumnNamesFromBase64(std::string messageBase64);
	bool				rbridge_columnUsedInFilter(const char * columnName);
	void				rbridge_findColumnsUsedInDataSet();
	std::string			rbridge_evalRCodeWhiteListed(std::string & rCode);

#endif // RBRIDGE_H
