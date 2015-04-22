#ifndef JASPEXPORTER_H
#define JASPEXPORTER_H

#include "datasetpackage.h"

#include <boost/function.hpp>

#include <string>
#include "libzip/archive.h"

class JASPExporter
{
public:
	static const Version jaspArchiveVersion;
	static const Version dataArchiveVersion;

	static void saveDataSet(const std::string &path, DataSetPackage* package, boost::function<void (const std::string &, int)> progressCallback);

private:
	static void saveDataArchive(archive *a, DataSetPackage *package, boost::function<void (const std::string &, int)> progressCallback);
	static void saveJASPArchive(archive *a, DataSetPackage *package, boost::function<void (const std::string &, int)> progressCallback);

	static void createJARContents(archive *a);
	static std::string getColumnTypeName(Column::ColumnType columnType);
};

#endif // JASPEXPORTER_H
