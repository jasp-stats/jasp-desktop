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

bool DataSetPackage::isModified()
{
	return _isModified;
}

void DataSetPackage::setLoaded()
{
	_isLoaded = true;
}

bool DataSetPackage::isLoaded()
{
	return _isLoaded;
}
