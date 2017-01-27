//
// Copyright (C) 2013-2016 University of Amsterdam
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

#ifndef __ODSIMPORTER_H_
#define __ODSIMPORTER_H_

#include "importer.h"
//#include "ods/odsdata.h"
#include "ods/odsimportdataset.h"
#include <boost/function.hpp>

#include <string>
#include <vector>

class ODSImporter : public Importer
{
public:
	//	static void loadDataSet(DataSetPackage *packageData, const std::string &locator, boost::function<void (const std::string &, int)> progressCallback);
	ODSImporter(DataSetPackage *packageData);
	virtual ~ODSImporter();

protected:
	// Implmemtation of Inporter base class.
	virtual ImportDataSet* loadFile(const std::string &locator, boost::function<void(const std::string &, int)> progressCallback);
	virtual void fillSharedMemoryColumn(ImportColumn *importColumn, Column &column);

private:
	static const std::string _contentFile;

	/**
	 * @brief readManifest Reads the ODS manifest.
	 * @param path The file path to the archive file
	 * @param dataset The data set to import into.

	 *
	 * After JaspImporter::readManifest.
	 */
	void readManifest(const std::string &path, ods::ODSImportDataSet *dataset);

	/**
	 * @brief readContents Reads contents to _dta;
	 * @param path The file path to the archive file
	 * @param dataset The data set to import into.
	 */
	void readContents(const std::string &path, ods::ODSImportDataSet *dataset);

};

#endif // end sentinal ODSIMPORTER_H
