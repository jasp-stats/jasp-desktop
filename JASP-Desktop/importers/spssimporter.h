//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef SPSSIMPORTER_H
#define SPSSIMPORTER_H

#include "importer.h"

//#include <boost/function.hpp>
//#include <boost/nowide/fstream.hpp>
#include <string>

#include "spss/systemfileformat.h"
#include "column.h"
#include "spss/spssimportcolumn.h"

namespace spss
{

/*
 * built with information from
 *
 * http://www.gnu.org/software/pspp/pspp-dev/html_node/System-File-Format.html
 */

class SPSSImporter : public Importer
{
public:
	SPSSImporter(DataSetPackage *packageData);
	virtual ~SPSSImporter();

	/**
	* @brief ReportProgress Reports progress for stream.
	* @param position Position to report.
	* @param progress report to here.
	*/
	void reportFileProgress(SPSSStream::pos_type position, boost::function<void (const std::string &, int)> progress);

	/**
	 * @brief resetNextCol reset the next col iterator
	 *
	 */
	void resetNextCol(SPSSImportDataSet *dataset);

	/**
	 * @brief getColumn Get next column wrapping as required.
	 * @return
	 */
	SPSSImportColumn& getNextColumn(SPSSImportDataSet *dataset);

	/**
	 * @brief isSpaning
	 * @return True if the last getColumn() call found a contination column.
	 */
	bool isSpaning() const { return _isSpaning; }

protected:
	virtual ImportDataSet* loadFile(const std::string &locator, boost::function<void(const std::string &, int)> progressCallback);
	virtual void fillSharedMemoryColumn(ImportColumn *importColumn, Column &column);

private:
	double						_fileSize = 0.0;
	ImportColumns::iterator		_currentColIter;
	size_t						_remainingColSpan;
	bool						_isSpaning;

	/**
	 * @brief _processStringsPostLoad - Delas with very Long strings (len > 255) and CP processes all strings.
	 * Call after the data is loaded!.
	 */
	void _processStringsPostLoad(SPSSImportDataSet* dataset, boost::function<void (const std::string &, int)> progress);

};

} // end namespace

#endif // SPSSIMPORTER_H
