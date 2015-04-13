#ifndef FILEPACKAGEDATA_H
#define FILEPACKAGEDATA_H

#include "dataset.h"
#include "lib_json/json.h"

#include "boost/signals2.hpp"

class FilePackageData
{
public:
	FilePackageData();

	DataSet *dataSet;
	std::string analysesHTML = std::string();
	Json::Value analysesData;
	bool hasAnalyses = false;

	void reset();
	void setAlteredState(bool value);
	bool alteredState();
	void declareLoaded();
	bool hasLoaded();

	boost::signals2::signal<void (FilePackageData *source)> alteredStateChange;

private:
	bool _alteredState = false;
	bool _hasLoaded = false;
};

#endif // FILEPACKAGEDATA_H
