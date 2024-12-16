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


#ifndef ENGINEBASE_H
#define ENGINEBASE_H

#include "dataset.h"

class EngineBase
{
public:
	EngineBase(unsigned long sessionID, bool useMemory = false);

	std::string				createColumn(				const std::string & columnName); ///< Returns encoded columnname on success or "" on failure (cause it already exists)
	bool					deleteColumn(				const std::string & columnName);
	bool					setColumnDataAndType(		const std::string & columnName, const	std::vector<std::string>	& nominalData, columnType colType); ///< return true for any changes
	int						getColumnType(				const std::string & columnName);
	int						getColumnAnalysisId(		const std::string & columnName);
	DataSet				*	provideAndUpdateDataSet();
	void					provideJaspResultsFileName(										std::string & root,	std::string & relativePath);
	void					provideStateFileName(											std::string & root,	std::string & relativePath);
	void					provideTempFileName(		const std::string & extension,		std::string & root,	std::string & relativePath);
	void					provideSpecificFileName(	const std::string & specificName,	std::string & root,	std::string & relativePath);
	int						dataSetRowCount()		{ return static_cast<int>(provideAndUpdateDataSet()->rowCount()); }

protected:
	bool					isColumnNameOk(const std::string & columnName);
	void					reloadColumnNames();

	DataSet				*	_dataSet		= nullptr;
	DatabaseInterface	*	_db				= nullptr;
	int						_analysisId		= -1;
};

#endif // ENGINEBASE_H
