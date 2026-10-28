//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
#ifndef RBRIDGE_H
#define RBRIDGE_H

#ifdef _WIN32
#undef Realloc
#undef Free
#endif

#include <string>
#include <functional>
#include <vector>
#include "jasprcpp_interface.h"
#include "columnencoder.h"
#include "databridge.h"		///< DataBridge::MultiDataSetSlice - the slice-queue producer types

class DataBridge;
class Filter;
class Workspace;

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
	RBridgeColumn*				STDCALL rbridge_readFullDataSet(				size_t * colMax);
	RBridgeColumn*				STDCALL rbridge_readDataSetRequested(			size_t * colMax, bool obeyFilter);
	RBridgeColumn*				STDCALL rbridge_readFullFilteredDataSet(		size_t * colMax);
	RBridgeColumn*				STDCALL rbridge_readFullDataSetHelper(			size_t * colMax, bool obeyFilter);
	RBridgeColumn*				STDCALL rbridge_readDataSetForFiltering(		size_t * colMax);
	RBridgeColumn*				STDCALL rbridge_readDataSetForFilteringCompCol(	size_t * colMax, bool obeyFilter);
	
	RBridgeColumn*				STDCALL rbridge_readDataSetForComputedColumns(	size_t * colMax);
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
	int							STDCALL rbridge_getColumnOriginalIndex	(const char * columnName);
	const char *				STDCALL rbridge_createColumn			(const char * columnName, bool computed);
	bool						STDCALL rbridge_deleteColumn			(const char * columnName);
	bool						STDCALL rbridge_setColumnDataAndType	(const char* columnName, const char **	nominalData,	size_t length,	int columnType, bool computed);
	bool						STDCALL rbridge_setDataSetData		(const char* datasetName, const char ** columnNames, const int * columnTypes, const char *** columnData, const size_t * columnLengths, size_t colCount);
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
	const char *				STDCALL rbridge_computedColumnFilterIs();
}

	typedef std::function<std::string (const std::string &, int progress)> RCallback;

	void				rbridge_setDataBridge(DataBridge * dataBridge);
	void				rbridge_clearDataBridge();
	void				rbridge_init(DataBridge * dataBridge, sendFuncDef sendToDesktopFunction, pollMessagesFuncDef pollMessagesFunction, const char * resultFont, bool insideJasp = true);

	void				rbridge_memoryCleaning();

	void				rbridge_setWantedCols(const ColumnEncoder::colsPlusTypes& datasetColsTypes);
	std::string			rbridge_runModuleCall(const std::string &name, const std::string &title, const std::string &moduleCall, const std::string &dataKey, const std::string &options, const std::string &stateKey, int analysisID, int analysisRevision, bool developerMode, ColumnEncoder::colsPlusTypes datasetColsTypes, bool preloadData, const std::string & multiDataSetJson = "");

	void				rbridge_setupRCodeEnvReadData(				const std::string & dataname, const std::string & readFunction);
	void				rbridge_setupRCodeEnv(		int rowCount,	const std::string & dataname = "data");
	void				rbridge_setupRCodeFilterEnv(int rowCount,	const std::string & dataname = "data");
	void				rbridge_detachRCodeEnv(						const std::string & dataname = "data");

	void				freeRBridgeColumns();
	void				rbridge_clearDataSet(); ///< Nulls rbridge_dataSet so it doesn't dangle after the engine unloads its workspace.
	void				freeRBridgeColumnDescription(RBridgeColumnDescription* columns, size_t colMax);
	void				freeLabels(char** labels, size_t nbLabels);

	// the filter/compute entry points take the run's EXPLICIT slice context - the
	//own filter (whose VIEW encodes the code's bare names, the host-bound fallback namespace, and
	//whose namespace the output name of a computed column encodes in) - and the transient
	//`referencedFilterIds` sidecar of the request (the desktop's scan of the code, consumed
	//for slice materialization; never seen by R). nullptr / empty keep the legacy behaviour:
	//the shown filter's view and no referenced slices, byte-identical to pre-Phase-7.
	std::vector<bool>	rbridge_applyFilter(					const std::string & filterCode, const std::string & generatedFilterCode, Filter * ownFilter = nullptr, const std::vector<int> & referencedFilterIds = {});
	std::string			rbridge_encodeColumnNamesInScript(		const std::string & filterCode, ColumnEncoder * sliceEncoder = nullptr);
	std::string			rbridge_evalRCodeWhiteListed(			const std::string & rCode, bool setWd, Filter * ownFilter = nullptr, const std::vector<int> & referencedFilterIds = {});
	std::string			rbridge_evalRComputedColumn(			const std::string & rCode, const std::string & setColumnCode, const std::string & filterName, Filter * ownFilter = nullptr, const std::vector<int> & referencedFilterIds = {});
	std::string			rbridge_evalRComputedDataSet(			const std::string & rCode, const std::string & outputDataSetName, const std::string & filterToUse, Filter * ownFilter = nullptr, const std::vector<int> & referencedFilterIds = {});

	/// Prepare the user R code of a filter/compute run that may
	/// carry qualified column references (the surface forms, scanned by the grammar
	/// helpers). Resolves every reference against `workspace`, rewrites it into the referenced
	/// filter view's unique token, and collects one read-slice per referenced FOREIGN filter
	/// (the columns the code references in each) plus the run's OWN slice LAST - the setup read
	/// pops it, so the own data frame is encoded in the run's own view too (the same view the
	/// bare names encode in). A self-reference resolves through the own view and queues no
	/// foreign slice (the own context IS the host-bound fallback namespace). Returns false + a
	/// ready-to-show `error` (the typed kinds; the loud-failure doctrine - the engine's error
	/// surface) when a reference does not resolve or names a filter the request did not list
	/// as a referenced slice. `ownFilter` is the run's own filter (the compute filter / the
	/// filter being run / the input filter of a computed dataset).
	bool				rbridge_prepareSliceRunScript(const std::string & code, Filter * ownFilter, Workspace * workspace, const std::vector<int> & referencedFilterIds, std::string & encodedScript, std::vector<DataBridge::MultiDataSetSlice> & slices, std::string & error);

	/// The new producer of slice queues for filter/compute
	/// runs - sets `slices` (foreign slices first, the run's own last) as the queue that
	/// rbridge_readDataSetRequested consumes (its slice handling unchanged), and attaches
	/// every FOREIGN slice as its own frame (`slice_<filterId>`) so the foreign tokens resolve
	/// on R's search path. The own slice stays queued for the setup read.
	void				rbridge_attachReferencedSlices(const std::vector<DataBridge::MultiDataSetSlice> & slices);
	///< Best-effort detach of the frames attached by rbridge_attachReferencedSlices.
	void				rbridge_detachReferencedSlices(const std::vector<DataBridge::MultiDataSetSlice> & slices);
	void				rbridge_setLANG(						const std::string & lang);
#endif // RBRIDGE_H
