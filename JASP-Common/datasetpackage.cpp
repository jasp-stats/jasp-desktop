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

bool DataSetPackage::isReady()
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

