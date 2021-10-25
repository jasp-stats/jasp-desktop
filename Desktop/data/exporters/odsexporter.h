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


	File created by patrick, on 01-11-2016
	Original file name was odsexporter.h
*/

#ifndef ODSEXPORTER_H
#define ODSEXPORTER_H

#include "exporter.h"

#include <QDomDocument>

///
/// To export to ODS
class ODSExporter : public Exporter
{
public:
	ODSExporter();


	/**
	 * @brief saveDataSet Saves the passed data set as an ODS file.
	 * @param path The path for the file.
	 * @param package The Data to save,
	 * @param progressCallback Called Back for progress.
	 */
	void saveDataSet(const std::string &path, DataSetPackage* package, boost::function<void (const std::string &, int)> progressCallback);

private:

	std::auto_ptr<QDomDocument> _currentDocument;

};

#endif // ODSEXPORTER_H
