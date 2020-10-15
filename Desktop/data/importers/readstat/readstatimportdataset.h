//
// Copyright (C) 2015-2018 University of Amsterdam
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

#ifndef ReadStatImportDataSet_H
#define ReadStatImportDataSet_H

#include <string>
#include <map>
#include "../importdataset.h"
#include "../readstatimporter.h"
#include "readstatimportcolumn.h"

//needed for the key of the submap in ReadStatImportDataSet::labelsMapT (otherwise it just compares the pointer probably)
bool operator<(const readstat_value_t & l, const readstat_value_t & r);

class ReadStatImportDataSet : public ImportDataSet
{
	typedef std::map<std::string, std::map<readstat_value_t, std::string>> labelsMapT;
public:
				ReadStatImportDataSet(ReadStatImporter * importer, boost::function<void(int)>	progressCallback)
					: ImportDataSet(importer), _progressCallback(progressCallback) {}

				~ReadStatImportDataSet()					override;

	int			var_count()						const	{ return _var_count; }
	void		setVariableCount(int newCount)				{ _var_count = newCount; }

	void						addLabelKeyValue(	const std::string & labelsID, const readstat_value_t & key, const std::string & label);
	const std::string		&	getLabel(			const std::string & labelsID, const readstat_value_t & key);
	void						setLabelsToColumns();

	void						addColumn(int index, ReadStatImportColumn * col); //Calls hidden virtual function addColumn(ImportColumn*)
	ReadStatImportColumn	*	column(int index);
	ReadStatImportColumn	*	operator[](int index) { return column(index); };

	void						setExpectedRows(int rows)	{ _expectedRows = rows; }
	void						setCurrentRow(int row);
	void						incrementRow()				{ setCurrentRow(_currentRow + 1); }

private:
	labelsMapT								_labelMap;
	int										_var_count			= 0;
	std::map<int,ReadStatImportColumn*>		_cols;
	int										_expectedRows		= 0,
											_currentRow			= 0;
	boost::function<void(int)>				_progressCallback;
};

#endif // ReadStatImportDataSet_H
