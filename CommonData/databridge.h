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

class ColumnEncoder;

class DataBridge
{
public:
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
};

#endif // DATABRIDGE_H
