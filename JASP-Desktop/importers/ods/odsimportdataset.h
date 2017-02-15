/*
	Copyright (C) Copyright (C) 2013-2017 University of Amsterdam

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
#include "odsimportcolumn.h"

#include <qstring.h>

namespace ods
{

/*
 * Functions as Sheet in the historical ODSData object.
 */
class ODSImportDataSet : public ImportDataSet
{
public:

	ODSImportDataSet();
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

	/**
	 * @brief minRow Gets the lowest numbered row in the sheet.
	 * @return The lowest numbered row in the sheet.
	 */
	int minRow() const;

	/**
	 * @brief maxRow  Gets the highest numbered row in the sheet.
	 * @return  The highest numbered row in the sheet.
	 */
	int maxRow() const;

	/**
	 * @brief createSpace Ensure that we have enough columns for the passed value.
	 * @param column The column number to check for.
	 * @return The number of columns available. - Maybe greater than column.
	 */
	size_t createSpace(int column);

	/**
	 * @brief operator [] Exposes the underlying vector of the ImportDataSet.
	 * @param index The bracketed value.
	 * @return A reference to the indexed value.
	 */
	ODSImportColumn & operator [] (const int index);

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
