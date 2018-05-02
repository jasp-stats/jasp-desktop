//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include <map>
#include "boost/signals2.hpp"
#include "jsonredirect.h"

#define DEFAULT_FILTER "# Here you can write your own filter using R directly.\n\n"\
	"# The grey readonly code above here created a logical vector generatedFilter\n"\
	"# generatedFilter is the result of the selected nominal/ordinal values,\n"\
	"# combined with the constructed formulas of the easy filterconstructor.\n\n"\
	"# By returning a logical vector, either by placing your selection at the end,\n"\
	"# or by using return(), you decide which rows will be selected.\n\n"\
	"# For example: to select only females with a testscore greater than 5:\n"\
	"# Gender == \"Female\" & TestScore > 5\n\n"\
	"# To combine this with the generated filter use: \n"\
	"# generatedFilter & Gender == \"Female\" & TestScore > 5\n\n"\
	"generatedFilter # returns the generated filter as the only filter"

class DataSetPackage
{
public:
	DataSetPackage();

	DataSet *dataSet = NULL;
	std::map<std::string, std::map<int, std::string> > emptyValuesMap;
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
	std::string dataFilter = DEFAULT_FILTER, filterConstructorJSON = "";

	bool refreshAnalysesAfterFilter = true;

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
