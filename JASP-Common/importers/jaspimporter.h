#ifndef JASPIMPORTER_H
#define JASPIMPORTER_H


#include "datasetpackage.h"

#include <boost/function.hpp>

#include <string>
#include <vector>

class JASPImporter
{
public:
	enum Compatibility { Compatible, Limited, IsAlpha, IsBeta, NotCompatible };

	static void loadDataSet(DataSetPackage *packageData, const std::string &path, boost::function<void (const std::string &, int)> progressCallback);

private:
	static void loadDataArchive(DataSetPackage *packageData, const std::string &path, boost::function<void (const std::string &, int)> progressCallback);
	static void loadJASPArchive(DataSetPackage *packageData, const std::string &path, boost::function<void (const std::string &, int)> progressCallback);
	static void loadDataArchive_1_00(DataSetPackage *packageData, const std::string &path, boost::function<void (const std::string &, int)> progressCallback);
	static void loadJASPArchive_1_00(DataSetPackage *packageData, const std::string &path, boost::function<void (const std::string &, int)> progressCallback);

	static Column::ColumnType parseColumnType(std::string name);
	static bool parseJsonEntry(Json::Value &root, const std::string &path, const std::string &entry, bool required);
	static void readManifest(DataSetPackage *packageData, const std::string &path);
	static Compatibility isCompatible(DataSetPackage *packageData);
};

#endif // JASPIMPORTER_H
