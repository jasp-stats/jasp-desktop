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

#ifndef FILEPACKAGE_H
#define FILEPACKAGE_H

#include "common.h"
#include "dataset.h"
#include "version.h"
#include "lib_json/json.h"

#include "boost/signals2.hpp"

class DataSetPackage
{
public:
	DataSetPackage();

	DataSet *dataSet = NULL;
	std::string analysesHTML;
	Json::Value analysesData;
	Version archiveVersion;
	Version dataArchiveVersion;
	bool isArchive = false;
	std::string warningMessage;

	std::string id;
	std::string initalMD5;

	std::string dataFilePath;
	uint dataFileTimestamp;
	bool dataFileReadOnly;

	bool hasAnalyses;

	void reset();
	void setModified(bool value);
	bool isModified() const;
	void setLoaded();
	bool isLoaded() const;
	bool isReady() const;
	void setWaitingForReady();
	void setAnalysesHTMLReady();

	boost::signals2::signal<void (DataSetPackage *source)> isModifiedChanged;
	boost::signals2::signal<void (DataSetPackage *source
								  , std::vector<std::string> &changedColumns
								  , std::vector<std::string> &missingColumns
								  , std::map<std::string, std::string> &changeNameColumns
								  )> dataChanged;

private:
	bool _isModified = false;
	bool _isLoaded = false;
	bool _analysesHTMLReady = false;
};

#endif // FILEPACKAGE_H
