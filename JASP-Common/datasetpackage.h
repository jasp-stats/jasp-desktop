#ifndef FILEPACKAGE_H
#define FILEPACKAGE_H

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

	bool hasAnalyses;

	void reset();
	void setModified(bool value);
	bool isModified() const;
	void setLoaded();
	bool isLoaded() const;

	boost::signals2::signal<void (DataSetPackage *source)> isModifiedChanged;

private:
	bool _isModified = false;
	bool _isLoaded = false;
};

#endif // FILEPACKAGE_H
