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
#include <boost/function.hpp>

#include "../JASP-Common/dataset.h"
#include "../JASP-R-Interface/jasprcpp_interface.h"

/* The R Bridge provides functions to the R analyses;
 * i.e. functions to read the data set from shared memory
 * Similarly, it provides functions to C++ code for
 * launching analyses written in R.
 * In this way, it functions as the bridge between the C++
 * application, and the R analyses
 */
extern "C" {
	RBridgeColumn* STDCALL rbridge_readDataSet(RBridgeColumnType* columns, int colMax);
	char** STDCALL rbridge_readDataColumnNames(int *colMax);
	RBridgeColumnDescription* STDCALL rbridge_readDataSetDescription(RBridgeColumnType* columns, int colMax);
	bool STDCALL rbridge_test(char** root);
	bool STDCALL rbridge_requestStateFileSource(const char **root, const char **relativePath);
	bool STDCALL rbridge_requestTempFileName(const char* extensionAsString, const char **root, const char **relativePath);
	bool STDCALL rbridge_runCallback(const char* in, int progress, const char** out);
}

	typedef boost::function<std::string (const std::string &, int progress)> RCallback;

	void rbridge_init();
	void rbridge_setFileNameSource(boost::function<void(const std::string &, std::string &, std::string &)> source);
	void rbridge_setStateFileSource(boost::function<void(std::string &, std::string &)> source);
	void rbridge_setDataSetSource(boost::function<DataSet *()> source);
	std::string rbridge_run(const std::string &name, const std::string &title, bool &requiresInit, const std::string &dataKey, const std::string &options, const std::string &resultsMeta, const std::string &stateKey, const std::string &perform = "run", int ppi = 96, RCallback callback = NULL);
	std::string rbridge_saveImage(const std::string &name, const std::string &type, const int &height, const int &width, const int ppi = 96);
	std::string rbridge_editImage(const std::string &name, const std::string &type, const int &height, const int &width, const int ppi = 96);

	std::string rbridge_check();

	void freeRBridgeColumns(RBridgeColumn *columns, int colMax);
	void freeRBridgeColumnDescription(RBridgeColumnDescription* columns, int colMax);
	void freeLabels(char** labels, int nbLabels);

#endif // RBRIDGE_H
