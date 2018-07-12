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
	RBridgeColumn*				STDCALL rbridge_readDataSet(RBridgeColumnType* columns, int colMax, bool obeyFilter);
	RBridgeColumn*				STDCALL rbridge_readFullDataSet(int * colMax);
	RBridgeColumn*				STDCALL rbridge_readDataSetForFiltering(int * colMax);
	RBridgeColumn*				STDCALL rbridge_readDataSetForEvalRCode(int * colMax);
	char**						STDCALL rbridge_readDataColumnNames(int *colMax);
	RBridgeColumnDescription*	STDCALL rbridge_readDataSetDescription(RBridgeColumnType* columns, int colMax);
	bool						STDCALL rbridge_test(char** root);
	bool						STDCALL rbridge_requestStateFileSource(const char **root, const char **relativePath);
	bool						STDCALL rbridge_requestJaspResultsFileSource(const char **root, const char **relativePath);
	bool						STDCALL rbridge_requestTempFileName(const char* extensionAsString, const char **root, const char **relativePath);
	const char*					STDCALL rbridge_requestTempRootName();
	bool						STDCALL rbridge_runCallback(const char* in, int progress, const char** out);
	void						STDCALL rbridge_setColumnAsScale		(const char* columnName, double *		scalarData,		size_t length);
	void						STDCALL rbridge_setColumnAsOrdinal		(const char* columnName, int *			ordinalData,	size_t length);
	void						STDCALL rbridge_setColumnAsNominal		(const char* columnName, int *			nominalData,	size_t length);
	void						STDCALL rbridge_setColumnAsNominalText	(const char* columnName, const char **	nominalData,	size_t length);
}

	typedef boost::function<std::string (const std::string &, int progress)> RCallback;

	void rbridge_init(sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction);
	void rbridge_setFileNameSource(boost::function<void(const std::string &, std::string &, std::string &)> source);
	void rbridge_setStateFileSource(boost::function<void(std::string &, std::string &)> source);
	void rbridge_setJaspResultsFileSource(boost::function<void(std::string &, std::string &)> source);
	void rbridge_setDataSetSource(boost::function<DataSet *()> source);

	void rbridge_setColumnDataAsScaleSource(		boost::function<void(std::string&, std::vector<double>&)>			source);
	void rbridge_setColumnDataAsOrdinalSource(		boost::function<void(std::string&, std::vector<int>&)>			source);
	void rbridge_setColumnDataAsNominalSource(		boost::function<void(std::string&, std::vector<int>&)>			source);
	void rbridge_setColumnDataAsNominalTextSource(	boost::function<void(std::string&, std::vector<std::string>&)>	source);

	std::string rbridge_run(const std::string &name, const std::string &title, const std::string &rfile, bool &requiresInit, const std::string &dataKey, const std::string &options, const std::string &resultsMeta, const std::string &stateKey, int analysisID, int analysisRevision, const std::string &perform = "run", int ppi = 96, RCallback callback = NULL, bool useJaspResults = false);
	std::string rbridge_check();


	void freeRBridgeColumns(RBridgeColumn *columns, int colMax);
	void freeRBridgeColumnDescription(RBridgeColumnDescription* columns, int colMax);
	void freeLabels(char** labels, int nbLabels);

	std::vector<bool>	rbridge_applyFilter(std::string & filterCode, std::string & generatedFilterCode);
	std::string			rbridge_encodeColumnNamesToBase64(std::string & filterCode);
	std::string			rbridge_decodeColumnNamesFromBase64(std::string messageBase64);
	bool				rbridge_columnUsedInFilter(const char * columnName);
	void				rbridge_findColumnsUsedInDataSet();
	std::string			rbridge_evalRCodeWhiteListed(std::string & rCode);

#endif // RBRIDGE_H
