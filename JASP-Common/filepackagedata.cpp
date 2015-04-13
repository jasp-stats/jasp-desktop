#include "filepackagedata.h"

FilePackageData::FilePackageData()
{
}

void FilePackageData::reset()
{
	dataSet = NULL;
	analysesHTML = std::string();
	analysesData = Json::arrayValue;
	hasAnalyses = false;
	_hasLoaded = false;
	setAlteredState(false);
}

void FilePackageData::setAlteredState(bool value)
{
	if (value != _alteredState)
	{
		_alteredState = value;
		alteredStateChange(this);
	}
}

bool FilePackageData::alteredState()
{
	return _alteredState;
}

void FilePackageData::declareLoaded()
{
	_hasLoaded = true;
}

bool FilePackageData::hasLoaded()
{
	return _hasLoaded;
}
