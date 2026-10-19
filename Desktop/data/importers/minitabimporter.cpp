#include "minitabimporter.h"
#include "data/importers/minitab/mwx.h"
#include "data/importers/minitab/minitabimportcolumn.h"
#include <QFileInfo>
#include "qutils.h"
#include <QDebug>


MinitabImporter::MinitabImporter() : Importer() 
{
}

ImportDataSet* MinitabImporter::loadFile(const std::string &locator, std::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(MinitabImporter::loadFile);

	ImportDataSet* data = new ImportDataSet(this);
	
	try 
	{
		Minitab minitab(locator);
		progressCallback(5);

		minitab.parseMwx();
		progressCallback(40);

		if (minitab.getColCount() == 0)
			throw std::runtime_error(fq(tr("The Minitab file contains no valid columns.")));

		std::vector<MwxImportColumn*> importColumns;
		importColumns.reserve(minitab.getColCount());
		
		minitab.getColumns(importColumns, data);
		progressCallback(90);

		for (MwxImportColumn* col : importColumns)
			data->addColumn(col);

		data->buildDictionary();
	}
	catch (const std::exception& e)
	{
		delete data;
			throw std::runtime_error(std::string("Minitab Import Error: ") + e.what());
	}

	progressCallback(100);
	JASPTIMER_STOP(MinitabImporter::loadFile);

	return data;
}
