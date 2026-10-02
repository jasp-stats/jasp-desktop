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


#ifndef DATABRIDGE_H
#define DATABRIDGE_H

#include "workspace.h"

#include <memory>
#include <vector>

class ColumnEncoder;

/// Ordered handout of the per-dataset slices of a multi-dataset aware analysis run: the Engine fills it,
/// rbridge_readDataSetRequested() consumes it one read per dataset (see DataBridge::takeMultiDataSetSlice).
class MultiDataSetSliceQueue
{
public:
	/// One dataset of a multi-dataset aware analysis run: which dataset, which of its filters to
	/// honour, and the columns+types (in that dataset's encoded names) the analysis requested from it.
	struct Slice
	{
		int								dataSetId	= -1;
		int								filterId	= -1;	///< -1: use the dataset's default filter
		ColumnEncoder::colsPlusTypes	cols;
	};

	/// Replace the whole queue (an empty vector disables the multi-dataset read-path again); the
	/// position restarts, so a stale queue can never leak into a next request.
	void	set(std::vector<Slice> queue)	{ _queue = std::move(queue); _pos = 0; }
	///< nullptr when no (further) slice is queued, advances the position otherwise.
	const Slice	*	take()					{ return _pos < _queue.size() ? &(_queue[_pos++]) : nullptr; }

private:
	std::vector<Slice>	_queue;
	size_t				_pos = 0;
};

class DataBridge
{
public:
	using MultiDataSetSlice = MultiDataSetSliceQueue::Slice;

	DataBridge(unsigned long sessionID, bool useMemory = false);
	~DataBridge();
	DataBridge(const DataBridge &) = delete;
	DataBridge & operator=(const DataBridge &) = delete;
	DataBridge(DataBridge &&) = delete;
	DataBridge & operator=(DataBridge &&) = delete;



	std::string				createColumn(				const std::string & columnName, bool computed=false); ///< Returns encoded columnname on success or "" on failure (cause it already exists)
	bool					deleteColumn(				const std::string & columnName);
	bool					setColumnDataAndType(		const std::string & columnName, const	std::vector<std::string>	& nominalData, columnType colType, bool computed); ///< return true for any changes
	bool					setDataSet(					const std::string & datasetName, const std::vector<std::string> & columnNames, const std::vector<columnType> & columnTypes, const std::vector<std::vector<std::string>> & columnData);
	int						getColumnType(				const std::string & columnName);
	int						getColumnAnalysisId(		const std::string & columnName);
	int						getColumnOriginalIndex(		const std::string & columnName);
	DataSet				*	provideAndUpdateDataSet(	int dataSetId = -1, std::function<void(float)> progressCallback = [](float){});
	void					provideJaspResultsFileName(										std::string & root,	std::string & relativePath);
	void					provideStateFileName(											std::string & root,	std::string & relativePath);
	void					provideTempFileName(		const std::string & extension,		std::string & root,	std::string & relativePath);
	void					provideSpecificFileName(	const std::string & specificName,	std::string & root,	std::string & relativePath);
	int						dataSetRowCount()		{ return static_cast<int>(provideAndUpdateDataSet()->rowCount()); }
	void 					updateOptionsAccordingToMeta(Json::Value & options);
	ColumnEncoder		*	extraEncodings()		{ return _extraEncodings.get(); }
	const ColumnEncoder	*	extraEncodings() const	{ return _extraEncodings.get(); }
	Workspace			*	workspace()				{ return resolveWorkspace(); }

	/// Load the per-dataset slices for the next analysis run (empty to disable the multi-dataset
	/// read-path again; every request sets this explicitly so a stale queue can never leak).
	void					setMultiDataSetQueue(std::vector<MultiDataSetSlice> queue)	{ _multiDataSetQueue.set(std::move(queue)); }
	const MultiDataSetSlice	*	takeMultiDataSetSlice()										{ return _multiDataSetQueue.take(); }

protected:
	bool					isColumnNameOk(const std::string & columnName);
	void					reloadColumnNames();
	///The Workspace to work against: adopts the process-wide one another owner made, or creates ours.
	Workspace			*	resolveWorkspace();
	///The DatabaseInterface to work against: adopts the process-wide one another owner made, or keeps the one we made ourselves.
	DatabaseInterface	*	resolveDb();


	Workspace			*	_workspace		= nullptr;
	bool					_ownsWorkspace	= false;	///< False when we adopted a Workspace that another owner (DataSetProvider, DataSetPackage) created.
	DatabaseInterface	*	_db				= nullptr;
	bool					_ownsDb			= false;	///< False when we adopted a DatabaseInterface that another owner (DataSetProvider, DataSetPackage) created.
	int						_analysisId		= -1;
	std::function<void()>	_datasetProvidedCallback;

private:
	static constexpr const char * ExtraOptionsPrefix = "JaspExtraOptions_";
	std::unique_ptr<ColumnEncoder>	_extraEncodings;
	MultiDataSetSliceQueue			_multiDataSetQueue;
};

#endif // DATABRIDGE_H
