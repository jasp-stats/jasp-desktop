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

#include "readstatimportdataset.h"
#include "log.h"
#include "utils.h"
#include "columnutils.h"

ReadStatImportDataSet::ReadStatImportDataSet(ReadStatImporter *importer, std::function<void (int)> progressCallback)
: ImportDataSet(importer), _progressCallback(progressCallback)
{

}

ReadStatImportDataSet::~ReadStatImportDataSet()
{
	_cols.clear(); //columns themselves get delete in ImportDataSet through the vector _columns
}

void ReadStatImportDataSet::addLabelKeyValue(const std::string & labelsID, const readstat_value_t & key, const std::string & label)
{
	//Log::log()		<< "addLabelKeyValue(labelsID='" << labelsID << "', key='" << std::flush;
	
	const std::string str = ReadStatImportColumn::readstatValueToString(key);
	
	//Log::log(false) << str << "', label='" << label << "')" << std::endl;

	_labelMap[labelsID][str] = label;
}

void ReadStatImportDataSet::addColumn(int index, ReadStatImportColumn * col)
{
	_cols[index] = col;
	ImportDataSet::addColumn(col);
}

ReadStatImportColumn * ReadStatImportDataSet::column(int index)
{
	if(_cols.count(index) > 0)	return _cols[index];
	else						return nullptr;
}

void ReadStatImportDataSet::setLabelsToColumns()
{
	for(auto & colKeyVal : _cols)
	{
		ReadStatImportColumn * col = colKeyVal.second;
		
		//Log::log() << "Setting labels for column " << col->name() << std::endl;		

		if(col->hasLabels())
			for(const auto & keyLabel : _labelMap[col->labelsID()])
				col->addLabel(keyLabel.first, keyLabel.second);
	}
}

void ReadStatImportDataSet::addNote(int note_index, const std::string &note)
{
	if(_notes.size() <= note_index)
		_notes.resize(note_index+1);

	_notes[note_index] = note;
}

const std::string & ReadStatImportDataSet::description() const
{
	static std::string localCache;

	localCache = ImportDataSet::description() + "\n" + stringUtils::join(_notes, "\n");

	return localCache;
}

void ReadStatImportDataSet::setCurrentRow(int row)
{
	if(_currentRow == row) return;

	_currentRow = row;

	_progressCallback(int(float(_currentRow) / float(_expectedRows) * 100.0));
}
