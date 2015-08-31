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
