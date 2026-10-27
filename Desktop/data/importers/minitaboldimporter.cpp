#include "minitaboldimporter.h"
#include "data/importers/minitabold/minitaboldimportcolumn.h"
#include "mtw.h" //resolves through the mtw_ffi imported target's include dir: Libraries/MTW-FFI/crates/mtw-ffi
#include "columnutils.h"
#include "log.h"
#include "qutils.h"

#include <cmath>
#include <limits>
#include <memory>
#include <set>
#include <vector>
#include <QFileInfo>

MinitabOldImporter::~MinitabOldImporter() {}

bool MinitabOldImporter::extSupported(const std::string & ext)
{
	return extsSupported().count(stringUtils::toLower(ext.length() > 0 && ext.substr(0, 1) == "." ? ext.substr(1) : ext)) > 0;
}

stringset MinitabOldImporter::extsSupported()
{
	static const std::set<std::string> supportedExts({"mtw", "mpj", "mtp"});

	return supportedExts;
}

namespace
{

///Closes the mtw-ffi document handle no matter how loadFile leaves it.
struct MtwDocGuard
{
	MtwDoc * doc;
	explicit	MtwDocGuard(MtwDoc * d) : doc(d) {}
				~MtwDocGuard()	{ mtw_close(doc); }
};

///The mtw-ffi handlers report failures as a negative error code, with the human
///readable message in the thread-local last-error slot. Turn that into the
///message JASP shows the user.
std::string mtwErrorToMessage(const std::string & whatHappened, int32_t errorCode)
{
	std::string message = mtw_last_error_message();

	return whatHappened + " (" + std::to_string(errorCode) + (message.empty() ? ")" : ": " + message + ")");
}

}

ImportDataSet* MinitabOldImporter::loadFile(const std::string &locator, std::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(MinitabOldImporter::loadFile);

	Log::log() << "MinitabOldImporter loads " << locator << std::endl;

	if(mtw_abi_version() != 1) //defensive: the submodule's mtw.h and the linked library must agree (CMake also probes this at configure time)
		throw std::runtime_error(fq(tr("The mtw-ffi library linked into JASP has ABI version %1, but this build expects version 1. Please reinstall or rebuild JASP.").arg(mtw_abi_version())));

	MtwDoc * doc = mtw_open(locator.c_str());

	if(!doc)
		throw std::runtime_error(fq(tr("Cannot read %1 as a Minitab file: %2").arg(tq(QFileInfo(tq(locator)).fileName().toStdString())).arg(tq(mtw_last_error_message()))));

	MtwDocGuard closeIt(doc);
	ImportDataSet * data = new ImportDataSet(this);

	try
	{
		//JASP loads one dataset per file (like the Excel importer reads the active
		//worksheet), so a project (.mpj) with several worksheets contributes its
		//first worksheet here. All worksheets of the file remain described by
		//mtw_metadata_json for tooling.
		int32_t sheet = 0;

		progressCallback(5);

		int32_t rowCount = mtw_row_count(doc, sheet),
				colCount = mtw_column_count(doc, sheet);

		if(rowCount < 0)	throw std::runtime_error(mtwErrorToMessage(fq(tr("Cannot read the number of rows of %1").arg(tq(locator))), -rowCount));
		if(colCount < 0)	throw std::runtime_error(mtwErrorToMessage(fq(tr("Cannot read the number of columns of %1").arg(tq(locator))), -colCount));
		if(colCount == 0)	throw std::runtime_error(fq(tr("The Minitab file contains no valid columns.")));

		//Read the shape of every column first: well-formed files have
		//mtw_column_row_count == mtw_row_count for each column, but a malformed
		//column that outgrew its worksheet wins, because its data exists. Every
		//column of the dataset gets the same length, the surplus rows are missing.
		std::vector<std::string>	colNames(colCount);
		std::vector<int32_t>		colKinds(colCount),
									colRows(colCount);
		size_t						dataSetRows = rowCount > 0 ? size_t(rowCount) : 0;

		for(int32_t col = 0; col < colCount; col++)
		{
			char * colName = mtw_column_name(doc, sheet, col);
			colNames[col] = colName ? colName : "";
			if(colName)		mtw_free(colName);

			colKinds[col] = mtw_column_kind(doc, sheet, col); //negative for the rare <unparsed> columns: they import as empty columns
			int32_t rows  = mtw_column_row_count(doc, sheet, col);
			colRows[col]  = rows < 0 ? rowCount : rows;

			if(colRows[col] > 0 && size_t(colRows[col]) > dataSetRows)
				dataSetRows = size_t(colRows[col]);
		}

		progressCallback(20);

		std::vector<MinitabOldImportColumn *> importColumns;
		importColumns.reserve(colCount);

		for(int32_t col = 0; col < colCount; col++)
		{
			columnType type = columnType::unknown;

			if(colKinds[col] == 1)		type = columnType::scale;	//numeric
			else if(colKinds[col] == 2)	type = columnType::nominal;	//text

			importColumns.push_back(new MinitabOldImportColumn(data, colNames[col], type));
			importColumns.back()->reserve(dataSetRows);
		}

		for(int32_t col = 0; col < colCount; col++)
		{
			MinitabOldImportColumn * column = importColumns[col];
			int32_t					rows	= colRows[col];

			if(colKinds[col] == 1) //numeric: one FFI call reads the whole column, missing cells arrive as NaN
			{
				std::vector<double> values((size_t(rows)));

				int32_t errorCode = mtw_read_numeric(doc, sheet, col, values.data(), rows);

				if(errorCode != 0)
					throw std::runtime_error(mtwErrorToMessage(fq(tr("Cannot read column %1 of %2").arg(tq(colNames[col])).arg(tq(locator))), -errorCode));

				for(int32_t row = 0; row < rows; row++)
					column->addValue(values[size_t(row)]);

				for(size_t row = size_t(rows); row < dataSetRows; row++) //a malformed column shorter than the worksheet
					column->addValue(std::numeric_limits<double>::quiet_NaN());
			}
			else if(colKinds[col] == 2) //text: cell by cell, blanks ("") stay blanks
			{
				for(int32_t row = 0; row < rows; row++)
				{
					const char * cell = mtw_read_text_cell(doc, sheet, col, row);

					column->addValue(cell ? cell : "");
				}

				for(size_t row = size_t(rows); row < dataSetRows; row++)
					column->addValue("");
			}
			else //empty (or unparsed) column: Minitab stores no data and (in practice) no name for it.
				//The cells are blanks, so a nameless one is dropped by buildDictionary exactly like a
				//completely blank csv column, and a named one imports as an all-missing column.
				for(size_t row = 0; row < dataSetRows; row++)
					column->addValue("");

			progressCallback(20 + int(70 * (col + 1) / colCount));
		}

		for(MinitabOldImportColumn * column : importColumns)
			data->addColumn(column);

		data->buildDictionary();
	}
	catch(const std::exception &)
	{
		delete data;
		throw;
	}

	progressCallback(100);
	JASPTIMER_STOP(MinitabOldImporter::loadFile);

	return data;
}
