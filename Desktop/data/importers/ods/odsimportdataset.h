/*
	Copyright (C) Copyright (C) 2013-2018 University of Amsterdam

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.


	File created by patrick, on 11-01-2017
	Original file name was
*/

#ifndef ODSIMPORTDATASET_H
#define ODSIMPORTDATASET_H

#include "../importdataset.h"
#include <qstring.h>


namespace ods
{

class ODSImporter;
class ODSImportColumn;

/*
 * Functions as Sheet in the historical ODSData object.
 */
class ODSImportDataSet : public ImportDataSet
{
public:

	ODSImportDataSet(ODSImporter* importer);
	virtual ~ODSImportDataSet();

	/*
	 * Getters and setters.
	 */
	/**
	 * @brief setContentFilename Sets content filename.
	 * @param filename Filename to set.
	 */
	void setContentFilename(const std::string &filename) { _contentFilename = filename; }

	/**
	 * @brief getContentFilename Gets content filename
	 * @return  content filename.
	 */
	const std::string &getContentFilename() const { return _contentFilename; }

	ODSImportColumn & createColumn(std::string name);

	/**
	 * @brief operator [] Exposes the underlying vector of the ImportDataSet.
	 * @param index The bracketed value.
	 * @return A reference to the indexed value.
	 */
	ODSImportColumn & operator [] (const int index);
	ODSImportColumn & getOrCreate (const int index);

	/**
	 * @brief postLoadProcess Performs post load processing.
	 */
	void postLoadProcess();


	static const std::string manifestPath; //< manfiest file in archive.
	static const QString contentRegExpression; //< A Reg. Ex. for the content filename.

private:
	std::string _contentFilename;
};

} // end namespace ods

#endif // ODSIMPORTDATASET_H
