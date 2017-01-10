//
// Copyright (C) 2017 University of Amsterdam
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

#include "datasetpackage.h"

DataSetPackage::DataSetPackage()
{
	hasAnalyses = false;
}

void DataSetPackage::reset()
{
	dataSet = NULL;
	archiveVersion = Version();
	dataArchiveVersion = Version();
	analysesHTML = std::string();
	analysesData = Json::arrayValue;
	hasAnalyses = false;
	warningMessage = std::string();
	_isLoaded = false;
	_analysesHTMLReady = false;
	setModified(false);
}

void DataSetPackage::setModified(bool value)
{
	if (value != _isModified)
	{
		_isModified = value;
		isModifiedChanged(this);
	}
}

bool DataSetPackage::isModified() const
{
	return _isModified;
}

void DataSetPackage::setLoaded()
{
	_isLoaded = true;
}

bool DataSetPackage::isLoaded() const
{
	return _isLoaded;
}

bool DataSetPackage::isReady() const
{
	return _analysesHTMLReady;
}


void DataSetPackage::setAnalysesHTMLReady()
{
	_analysesHTMLReady = true;
}

void DataSetPackage::setWaitingForReady()
{
	_analysesHTMLReady = false;
}

