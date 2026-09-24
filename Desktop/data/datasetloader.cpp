//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
#include "datasetloader.h"

#include <boost/algorithm/string.hpp>

#include "importers/databaseimporter.h"
#include "importers/csvimporter.h"
#include "importers/jaspimporter.h"
#include "importers/odsimporter.h"
#include "importers/readstatimporter.h"
#include "importers/excelimporter.h"
#include "importers/rdataimporter.h"
#include "importers/minitabimporter.h"
#include "asyncloader.h"

#include <QFileInfo>
#include <QScopeGuard>
#include <memory>

#include "timers.h"
#include "utils.h"
#include "log.h"
#include "utilities/desktopcommunicator.h"
#include "utilities/qutils.h"
#include "datasetpackage.h"

using namespace std;
using namespace ods;

string DataSetLoader::getExtension(const string &locator, const string &extension)
{
	std::filesystem::path path(locator);
	string ext = path.extension().generic_string();

	if (!ext.length()) ext=extension;
	return ext;
}

Importer* DataSetLoader::getImporter(const string & locator, const string &ext)
{
	if(	ext == "DATABASE")									return new DatabaseImporter();
	if(	boost::iequals(ext,".csv") || 
		boost::iequals(ext,".txt") ||
		boost::iequals(ext,".tsv"))							return new CSVImporter();
	if(	boost::iequals(ext,".ods"))							return new ODSImporter();
	if( boost::iequals(ext,".xls") ||
		boost::iequals(ext,".xlsx"))						return new ExcelImporter();
	if(	ReadStatImporter::extSupported(ext))				return new ReadStatImporter(ext);
	if( boost::iequals(ext,".rdata") ||
		boost::iequals(ext,".rds"))							return new RDataImporter();
	if( boost::iequals(ext, ".mwx") ||
		boost::iequals(ext,".mpx"))							return new MinitabImporter();

	return nullptr; //If NULL then JASP will try to load it as a .jasp file (if the extension matches)
}

///The delimiter and locale of a csv reach its importer through DesktopCommunicator, and hold for that one load or sync only. So they are forgotten
///afterwards, whichever way it ends: left behind, the next csv opened would not show the preview but take them over (see DesktopCommunicator::askCsvDelimiter)
static auto forgetCsvChoicesAfterwards()
{
	return qScopeGuard([]
	{
		DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');
		DesktopCommunicator::singleton()->setKnownImportLocale(std::nullopt);
	});
}

void DataSetLoader::loadPackage(const string &locator, const string &extension, std::function<void(int)> progress)
{
	JASPTIMER_RESUME(DataSetLoader::loadPackage);

	//Also after a .jasp file: a delimiter can be known beforehand, to open a csv without its preview (see data_load in MainWindow)
	auto forgetThem = forgetCsvChoicesAfterwards();

	std::unique_ptr<Importer> importer(getImporter(locator, extension)); //Also freed when the load fails, with the ImportDataSet it holds (a child of it)

	if (importer)
	{
		DesktopCommunicator * communicator = DesktopCommunicator::singleton();

		importer->loadDataSet(locator, progress);

		//Remember what the csv preview chose, so that synchronising the data later reads the file the same way again
		const char						delimiter	= communicator->knownCsvDelimiter();
		const std::optional<QLocale>	locale		= communicator->knownImportLocale();

		if ((delimiter != '\0' || locale) && DataSetPackage::pkg()->dataSet())
			DataSetPackage::pkg()->dataSet()->setCsvChoices(delimiter, locale ? fq(locale->bcp47Name()) : "");
	}
	else if(extension == ".jasp" || extension == "jasp")
		JASPImporter::loadDataSet(locator, progress);
	else
		throw LoaderException("JASP does not support loading the file-type \"" + extension + '"');

	JASPTIMER_STOP(DataSetLoader::loadPackage);

}

void DataSetLoader::syncPackage(const string &locator, const string &extension, std::function<void(int)> progress)
{
	std::unique_ptr<Importer> importer(getImporter(locator, extension)); //Also freed when the sync fails

	if (importer)
	{
		DesktopCommunicator	*	communicator	= DesktopCommunicator::singleton();
		const DataSet		*	dataSet			= DataSetPackage::pkg()->dataSet();
		auto					forgetThem		= forgetCsvChoicesAfterwards();

		//Read the file the way the csv preview chose when the data was imported: split differently, or with its numbers read in another locale,
		//a sync would silently change the data. That holds for any file synchronised into this data, a batch run feeds its template files like the first.
		communicator->setKnownCsvDelimiter(dataSet ? dataSet->csvDelimiter() : '\0');
		communicator->setKnownImportLocale(dataSet && !dataSet->importLocale().empty() ? std::optional<QLocale>(QLocale(tq(dataSet->importLocale()))) : std::nullopt);

		importer->syncDataSet(locator, progress);
	}
}
