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
#include "testall.h"
#include "testinfo.h"
#include "numbersinlocales.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "utilities/qutils.h"
#include "databaseinterface.h"
#include "utilities/settings.h"
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"
#include "data/importers/odsimporter.h"
#include "data/importers/jaspimporter.h"
#include "data/exporters/jaspexporter.h"
#include "data/importers/excelimporter.h"
#include "data/importers/rdataimporter.h"

#include "data/importers/readstatimporter.h"
#include "data/importers/minitabimporter.h"
#include "data/importers/database/databaseimportcolumn.h"
#include "utilities/settings.h"
#include "gui/preferencesmodel.h"
#include "utilities/desktopcommunicator.h"
#include "rpc/jasprpcdispatcher.h"
#include "rpc/jasprpcserver.h"
#include "ai/agentstatetracker.h"
#include <QNetworkAccessManager>
#include <QNetworkProxy>
#include <QNetworkReply>
#include <QTemporaryDir>
#include <QScopeGuard>
#include "columnutils.h"
#include "appinfo.h"
#include <cmath>
#include "dataset.h"
#include "data/asyncloader.h"
#include "data/datasetloader.h"
#include "mainwindow.h"
#include "results/resultsjsinterface.h"

#include <QSignalSpy>
#include <QFile>
#include <QFileInfo>
#include <QEventLoop>
#include <QTimer>
#include <sqlite3.h>
#include <archive.h>
#include <archive_entry.h>


void TestAll::initTestCase()
{
	TempFiles::init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory
}

void TestAll::init()
{
	Settings::informSettingsThatThisIsATest();
	//_pkg->reset(false);
}

void TestAll::cleanup()
{

	delete _importer;
	_importer = nullptr;

	//After a test tore down a full MainWindow the session database is gone with it, so the
	//DatabaseInterface the singleton() call would lazily recreate here cannot load anything anymore.
	//closeInterfaces() alone is enough: deleting a null pointer is fine and the destructor closes.
	try
	{
		DatabaseInterface::closeInterfaces();
	}
	catch(const std::exception & e)
	{
		std::cerr << "TestAll::cleanup: skipping database teardown: " << e.what() << std::endl;
	}
	delete _pkg;
	_pkg = nullptr;
}

#define TO_STR2(x) #x
#define TO_STR(x) TO_STR2(x)


void TestAll::testDataImport_data()
{
	QTest::addColumn<QString>("folder");
	QTest::addColumn<QString>("dataFileAbsolutePath");

	for(const QString & folder : _testLibrary().entryList(QDir::Filter::Dirs | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
	{
		if(folder == "jasp")
			continue;

		QDir subDir(_testLibrary());
		subDir.cd(folder);

		for(QFileInfo & i : subDir.entryInfoList(QDir::Filter::Files | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
			if(i.suffix() != "json")
				QTest::newRow(i.fileName().toUtf8()) << folder << i.absoluteFilePath();
	}
}

void TestAll::testDataImport()
{
	QFETCH(QString, folder);
	QFETCH(QString, dataFileAbsolutePath);

	QDir subDir(_testLibrary());
	subDir.cd(folder);

	auto getImporter = [&]() -> Importer *
	{
		if(folder == "readstat")	return new ReadStatImporter();
		if(folder == "rdata")		return new RDataImporter();
		if(folder == "excel")		return new ExcelImporter();
		if(folder == "ods")			return new ods::ODSImporter();
		if(folder == "csv")			return new CSVImporter(false);

		return nullptr;
	};

	if(_pkg)
		delete _pkg;

	if(_importer)
		delete _importer;

	_pkg = new DataSetPackage(this);
	_importer = getImporter();

	QVERIFY2(_importer, "Getting importer failed...");

	std::cerr << "Testing " << dataFileAbsolutePath << std::endl;
	_importer->loadDataSet(fq(dataFileAbsolutePath), [](int i){});

	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet,						"No dataset!");

	Json::Value compareMe = dataSet->jsonForCompare();

	QString jsonFilePath = dataFileAbsolutePath,
			ext			 = QFileInfo(dataFileAbsolutePath).suffix();

	jsonFilePath.replace(jsonFilePath.size() - (ext.size() + 1), ext.size() + 1, ".json");

	QFileInfo jsonFileIn(jsonFilePath);

	if(!jsonFileIn.exists())
	{
		std::cerr << "Json does not exist yet, creating it now!" << std::endl;
		QFile jsonFile(jsonFilePath);
		jsonFile.open(QFile::OpenModeFlag::WriteOnly);
		jsonFile.write(compareMe.toStyledString().c_str());
		jsonFile.close();
	}

	QVERIFY(jsonFileIn.exists());

	QFile jsonFile(jsonFilePath);

	jsonFile.open(QFile::OpenModeFlag::ReadOnly);

	std::string jsonTxt  = fq(jsonFile.readAll());

	Json::Reader parser;
	Json::Value  hardcoded;

	QVERIFY2(parser.parse(jsonTxt, hardcoded),	"Parsing json failed!");

	bool hardcodedIsSame = hardcoded == compareMe;

	if(!hardcodedIsSame)
	{
		std::cerr << stringUtils::replaceBy(compareMe.toStyledString(), "\n", " ") << std::endl;
		std::cerr << fq(dataFileAbsolutePath) + " Test fails" << std::endl;
	}

	QVERIFY2(hardcodedIsSame, "Hardcoded json is different!" );


	DataSet loadMe(dataSet->id());
	QVERIFY2(dataSet->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}


void TestAll::testJaspDataImport_data()
{
	QTest::addColumn<QString>("folder");
	QTest::addColumn<QString>("dataFileAbsolutePath");

	for(const QString & folder : _testLibrary().entryList(QDir::Filter::Dirs | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
	{
		if(folder != "jasp")
			continue;

		QDir subDir(_testLibrary());
		subDir.cd(folder);

		for(QFileInfo & i : subDir.entryInfoList(QDir::Filter::Files | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
			if(i.suffix() != "json")
				QTest::newRow(i.fileName().toUtf8()) << folder << i.absoluteFilePath();
	}
}

void TestAll::testJaspRoundRobin_data()
{
	testJaspDataImport_data();
}

void TestAll::testJaspRoundRobin()
{
	QFETCH(QString, folder);
	QFETCH(QString, dataFileAbsolutePath);

	QDir subDir(_testLibrary());
	subDir.cd(folder);

	if(_pkg)
		delete _pkg;

	if(_importer)
		delete _importer;

	_pkg = new DataSetPackage(this);

	std::cerr << "Testing " << dataFileAbsolutePath << std::endl;
	JASPImporter::loadDataSet(fq(dataFileAbsolutePath),		[](int){});

	DataSet *	dataSet		= _pkg->dataSet();
	QVERIFY2(dataSet,			"No dataset!");

	Json::Value compareMe	= dataSet->jsonForCompare();
	std::string jaspFile	= TempFiles::createSpecific("testjasp", "temp.jasp");

	std::cerr << "Storing jasp file temporarily to: " << jaspFile << std::endl;
	// Create snapshot before exporting
	JASPExporter::createSnapshot("testjasp_snapshot_");
	JASPExporter().saveDataSet(jaspFile, [](int){});

	_pkg->reset();
	QVERIFY2(_pkg->dataSet()->jsonForCompare() != compareMe, "DataSet should be different after resetting DataSetPackage!");

	JASPImporter::loadDataSet(jaspFile, [](int){});

	dataSet = _pkg->dataSet();
	QVERIFY2(dataSet,									"No dataset!");
	QVERIFY2(dataSet->jsonForCompare() == compareMe,	"DataSet should be the same after reloading!");
}


void TestAll::testJaspDataImport()
{
	QFETCH(QString, folder);
	QFETCH(QString, dataFileAbsolutePath);

	QDir subDir(_testLibrary());
	subDir.cd(folder);

	if(_pkg)
		delete _pkg;

	if(_importer)
		delete _importer;

	_pkg = new DataSetPackage(this);

	std::cerr << "Testing " << dataFileAbsolutePath << std::endl;

	JASPImporter::loadDataSet(fq(dataFileAbsolutePath),		[](int){});

	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet,						"No dataset!");

	Json::Value compareMe = dataSet->jsonForCompare();

	QString jsonFilePath = dataFileAbsolutePath,
			ext			 = QFileInfo(dataFileAbsolutePath).suffix();

	jsonFilePath.replace(jsonFilePath.size() - (ext.size() + 1), ext.size() + 1, ".json");

	QFileInfo jsonFileIn(jsonFilePath);

	if(!jsonFileIn.exists())
	{
		std::cerr << "Json does not exist yet, creating it now!" << std::endl;
		QFile jsonFile(jsonFilePath);
		jsonFile.open(QFile::OpenModeFlag::WriteOnly);
		jsonFile.write(compareMe.toStyledString().c_str());
		jsonFile.close();

	}

	QVERIFY(jsonFileIn.exists());

	QFile jsonFile(jsonFilePath);


	jsonFile.open(QFile::OpenModeFlag::ReadOnly);

	std::string jsonTxt  = fq(jsonFile.readAll());

	Json::Reader parser;
	Json::Value  hardcoded;

	QVERIFY2(parser.parse(jsonTxt, hardcoded),	"Parsing json failed!");

	bool hardcodedIsSame = hardcoded == compareMe;

	if(!hardcodedIsSame)
		std::cerr << stringUtils::replaceBy(compareMe.toStyledString(), "\n", " ") << std::endl;

	QVERIFY2(hardcodedIsSame,			"Hardcoded json is different!");


	DataSet loadMe(dataSet->id());
	QVERIFY2(dataSet->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

///Cancelling the csv preview aborts the import from inside loadDataSet, which is after beginLoadingData() opened a model reset.
///If that reset is never closed the whole of JASP draws nothing at all, which is what a blank window after Cancel comes down to.
void TestAll::testCancelledImportLeavesTheModelUsable()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(true);	//true: this one asks for a delimiter, so it can be cancelled

	QSignalSpy resetStarted(	_pkg, &QAbstractItemModel::modelAboutToBeReset);
	QSignalSpy resetFinished(	_pkg, &QAbstractItemModel::modelReset);

	//When the model shows again the aborted import must already be gone, so nothing can look at it while it is deleted
	bool hadDataSetWhenShownAgain = false;
	QMetaObject::Connection watching = connect(_pkg, &QAbstractItemModel::modelReset, this, [&]{ hadDataSetWhenShownAgain = _pkg->hasDataSet(); });

	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0'); //Otherwise it would not ask at all

	//Answering with '\0' is what pressing Cancel in the preview window comes down to
	QMetaObject::Connection cancelling = connect(DesktopCommunicator::singleton(), &DesktopCommunicator::askCsvDelimiterSignal,
		[](const QString &, char) { DesktopCommunicator::singleton()->delimiterChosen('\0'); });

	QDir csvDir(_testLibrary());
	csvDir.cd("csv");

	bool threw = false;

	try
	{
		_importer->loadDataSet(fq(csvDir.absoluteFilePath("Data Import Test CSV.csv")), [](int){});
	}
	catch(const std::exception &)
	{
		threw = true; //"No data loaded", the way an aborted import always ends
	}

	disconnect(cancelling);
	disconnect(watching);

	QVERIFY2(threw, "cancelling the preview should abort the import");
	QVERIFY2(resetStarted.count() > 0, "the import should have started a model reset at all");
	QCOMPARE(resetFinished.count(), resetStarted.count()); //Every begin got its end, so the model is usable again
	QVERIFY2(!hadDataSetWhenShownAgain, "the aborted import should be thrown away before the model shows again");
}

///A NULL in a numeric column of a database is missing, not 0: PostgreSQL, MySQL and ODBC hand it over as a number that is not there
void TestAll::testDatabaseImportNulls()
{
	DatabaseImportColumn column(nullptr, "x", QMetaType::fromType<double>());

	column.addValue(QVariant(1.5));
	column.addValue(QVariant(QMetaType::fromType<double>()));	//That NULL

	QCOMPARE(column.valueLookup(0), std::string("1.5"));
	QCOMPARE(column.valueLookup(1), std::string(""));
}

///Has (Q)ColumnUtils read and write numbers the way JASP does with its interface set to locale, until what is returned goes out of scope.
///The tests here otherwise run without any locale of the interface, just like the numbers in C.
static auto interfaceLocale(const QLocale & locale, bool useThousandSeparators = false)
{
	const QLocale defaultBefore;

	QColumnUtils::setCallbacksAndDefaultLocale(locale, useThousandSeparators);

	return qScopeGuard([defaultBefore]
	{
		QLocale::setDefault(defaultBefore);
		ColumnUtils::setCurrentQLocaleId("C");
		ColumnUtils::setDecimalPoint(".");
		ColumnUtils::setAlternativeDoubleToString(nullptr, nullptr);
		ColumnUtils::setExtraStringToNumber(nullptr, nullptr);
	});
}

///Has a csv import take ';' as delimiter and read its numbers as picked (nothing picked: the way the interface does),
///as if the csv preview just closed, until what is returned goes out of scope
static auto csvPreviewPicked(std::optional<QLocale> picked)
{
	DesktopCommunicator::singleton()->setKnownCsvDelimiter(';');
	DesktopCommunicator::singleton()->setKnownImportLocale(picked);

	return qScopeGuard([]
	{
		DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');
		DesktopCommunicator::singleton()->setKnownImportLocale(std::nullopt);
	});
}

///The locale picked in the csv preview decides how the numbers of that file are read, whatever the locale of the interface:
///"1,234" in a German file is one point two three four, and "1,234.56" is no German number at all even though English reads it fine.
void TestAll::testCsvImportLocale()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);	//false: take the delimiter below instead of asking for it

	QTemporaryDir	dir;
	QFile			csv(dir.filePath("german.csv"));
	QVERIFY(csv.open(QIODevice::WriteOnly));
	csv.write("x;y\n1,234;1,234.56\n86,298;2\n0,5;3\n1.234,56;4\n");
	csv.close();

	auto english = interfaceLocale(QLocale(QLocale::English, QLocale::UnitedStates));

	auto german = csvPreviewPicked(QLocale(QLocale::German, QLocale::Germany));

	_importer->loadDataSet(fq(csv.fileName()), [](int){});

	QVERIFY(_pkg->dataSet() && _pkg->dataSet()->columnCount() == 2);

	const doublevec & values = _pkg->dataSet()->column(0)->dbls();

	QCOMPARE(values.size(),	size_t(4));
	QCOMPARE(values[0],		1.234);
	QCOMPARE(values[1],		86.298);
	QCOMPARE(values[2],		0.5);
	QCOMPARE(values[3],		1234.56);

	QCOMPARE(_pkg->dataSet()->column(1)->getValue(0), std::string("1,234.56")); //Text, not the number the interface would make of it
}

///A csv with a locale of its own hands its numbers over written the way C does (see CSVImportColumn), but the data shows them the way the interface
///writes them, so that is the width the column needs: with thousand separators 1234567.5 takes up 11 characters, not 9
void TestAll::testCsvImportLocaleColumnWidth()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	QFile			csv(dir.filePath("german.csv"));
	QVERIFY(csv.open(QIODevice::WriteOnly));
	csv.write("x\n1234567,5\n");
	csv.close();

	auto english = interfaceLocale(QLocale(QLocale::English, QLocale::UnitedStates), true);

	auto german = csvPreviewPicked(QLocale(QLocale::German, QLocale::Germany));

	_importer->loadDataSet(fq(csv.fileName()), [](int){});

	QVERIFY(_pkg->dataSet() && _pkg->dataSet()->columnCount() == 1);

	Column * column = _pkg->dataSet()->column(0);

	QCOMPARE(column->getValue(0),										std::string("1,234,567.5"));
	QCOMPARE(column->getMaximumWidthInCharacters(true, true, 0),		size_t(11));
}

///A locale that groups thousands with a space reads numbers grouped with any kind of space: French prescribes U+202F and Russian U+00A0,
///but a file has whichever one the software that wrote it knew, or the plain space of a keyboard
void TestAll::testNumbersGroupedWithSpaces()
{
	const QList<QChar> spaces = { u' ', QChar(0x00A0), QChar(0x202F), QChar(0x2009) }; //Plain, no-break, narrow no-break and thin

	for(const QLocale & locale : { QLocale(QLocale::French, QLocale::France), QLocale(QLocale::French, QLocale::Canada), QLocale(QLocale::Russian, QLocale::Russia), QLocale(QLocale::Swedish, QLocale::Sweden) })
	{
		auto interfaceIsSet	= interfaceLocale(locale); //For reading whole numbers, ColumnUtils::getIntValue
		auto readNumbersAs	= QColumnUtils::stringToDoubleFor(locale);

		for(QChar space : spaces)
		{
			const QString	wholeNumber	= "1" + QString(space) + "234",
							number		= wholeNumber + locale.decimalPoint() + "5",
							what		= " in " + locale.bcp47Name() + " with U+" + QString::number(space.unicode(), 16).toUpper().rightJustified(4, u'0');
			double			read		= 0;
			int				readWhole	= 0;

			QVERIFY2(readNumbersAs(fq(number), read) && read == 1234.5,						qPrintable(number		+ what));
			QVERIFY2(ColumnUtils::getIntValue(fq(wholeNumber), readWhole) && readWhole == 1234,	qPrintable(wholeNumber	+ what));
		}
	}

	//A locale that groups with something else does not take a space for its group separator
	double read;
	QVERIFY(!QColumnUtils::stringToDoubleFor(QLocale(QLocale::English, QLocale::UnitedStates))(fq("1" + QString(QChar(0x00A0)) + "234.5"), read));
}

///Writes NumbersInLocales::samples() to a csv, each in a column of its own so that no sample decides what the others are, rows times
static QString writeNumberSamples(const QTemporaryDir & dir, int rows = 1)
{
	const std::vector<NumbersInLocales::Sample> & samples = NumbersInLocales::samples();

	QByteArray names, values;

	for(size_t i=0; i<samples.size(); i++)
	{
		names	+= (i ? ";" : "") + QByteArray("sample") + QByteArray::number(qulonglong(i));
		values	+= (i ? ";" : "") + QByteArray(samples[i].written);
	}

	QFile csv(dir.filePath("numbers.csv"));

	if(!csv.open(QIODevice::WriteOnly))
		return "";

	csv.write(names + "\n" + QByteArray(values + "\n").repeated(rows));

	return csv.fileName();
}

///Where a sample is in the data (see writeNumberSamples), for a message
static QString sampleAt(const NumbersInLocales::Sample & sample, size_t row)
{
	return QString("\"%1\" in row %2").arg(QString::fromUtf8(sample.written)).arg(row + 1);
}

///Every sample in every row of the data (see writeNumberSamples) that is not the number, or the text, it is in the language readIn
static QStringList numberSamplesReadWrong(DataSet * data, const QLocale & readIn)
{
	const std::vector<NumbersInLocales::Sample> & samples = NumbersInLocales::samples();

	if(!data || data->columnCount() != int(samples.size()))
		return { "the data should have a column for every sample" };

	QStringList readWrong;

	for(size_t i=0; i<samples.size(); i++)
		for(size_t row=0; row<data->column(i)->rowCount(); row++)
		{
			const double	number		= samples[i].readIn(readIn);
			double			read		= 0;
			const bool		readNumber	= data->column(i)->numberAt(row, read);

			if(std::isnan(number) ? readNumber : !readNumber || !qFuzzyCompare(read, number))
				readWrong.push_back(QString("%1 should be %2, but is %3").arg(sampleAt(samples[i], row), std::isnan(number) ? "text" : QString::number(number, 'g', 12), readNumber ? QString::number(read, 'g', 12) : "text"));
		}

	return readWrong;
}

///Every sample in every row of the data (see writeNumberSamples) that is not shown the way the interface shows what it is in the language readIn
static QStringList numberSamplesShownWrong(DataSet * data, const QLocale & readIn, const QLocale & interface, bool thousandSeparators)
{
	const std::vector<NumbersInLocales::Sample> & samples = NumbersInLocales::samples();

	if(!data || data->columnCount() != int(samples.size()))
		return { "the data should have a column for every sample" };

	QStringList shownWrong;

	for(size_t i=0; i<samples.size(); i++)
	{
		Column		*	column	= data->column(i);
		const double	number	= samples[i].readIn(readIn);

		//Text stays as it was written, and a column of whole numbers comes out nominal, where the label writes its number plainly: without thousand separators
		const QString	shouldShow	= std::isnan(number)					? QString::fromUtf8(samples[i].written)
									: column->type() == columnType::scale	? NumbersInLocales::shownIn(interface, number, thousandSeparators)
																			: QString::number(number, 'g', 10);

		for(size_t row=0; row<column->rowCount(); row++)
		{
			const QString shows = tq(column->getValue(row));

			if(shows != shouldShow)
				shownWrong.push_back(QString("%1 should show as \"%2\", but shows as \"%3\"").arg(sampleAt(samples[i], row), shouldShow, shows));
		}
	}

	return shownWrong;
}

///Every combination of an interface in English, German or French, with and without thousand separators, and a csv whose numbers are written in
///English, German or French, or in a language nobody picked (the csv preview was never shown, as when synchronising a file imported before it existed)
void TestAll::testCsvImportNumbers_data()
{
	QTest::addColumn<QLocale>(	"interface");
	QTest::addColumn<bool>(		"thousandSeparators");
	QTest::addColumn<bool>(		"picked");
	QTest::addColumn<QLocale>(	"readIn");

	for(const QLocale & interface : NumbersInLocales::locales())
		for(bool thousandSeparators : { false, true })
		{
			const QString interfaceIs = QLocale::languageToString(interface.language()) + " interface" + (thousandSeparators ? " with thousand separators" : "");

			for(const QLocale & file : NumbersInLocales::locales())
				QTest::addRow("%s, %s file", qPrintable(interfaceIs), qPrintable(QLocale::languageToString(file.language())))
					<< interface << thousandSeparators << true << file;

			//Nothing picked: read the way the interface reads
			QTest::addRow("%s, nothing picked", qPrintable(interfaceIs))
				<< interface << thousandSeparators << false << interface;
		}
}

///A number is read in the language of its file and shown in the language of the interface, whichever combination of the two it is
void TestAll::testCsvImportNumbers()
{
	QFETCH(QLocale,	interface);
	QFETCH(bool,	thousandSeparators);
	QFETCH(bool,	picked);
	QFETCH(QLocale,	readIn);

	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	const QString	csv = writeNumberSamples(dir);
	QVERIFY(!csv.isEmpty());

	auto interfaceIsSet	= interfaceLocale(interface, thousandSeparators);
	auto fileIsPicked	= csvPreviewPicked(picked ? std::optional<QLocale>(readIn) : std::nullopt);

	_importer->loadDataSet(fq(csv), [](int){});

	const QStringList	readWrong	= numberSamplesReadWrong(	_pkg->dataSet(), readIn),
						shownWrong	= numberSamplesShownWrong(	_pkg->dataSet(), readIn, interface, thousandSeparators);

	QVERIFY2(readWrong	.isEmpty(), qPrintable("\n" + readWrong	.join("\n")));
	QVERIFY2(shownWrong	.isEmpty(), qPrintable("\n" + shownWrong	.join("\n")));
}

void TestAll::testCsvSyncNumbers_data()
{
	testCsvImportNumbers_data();
}

///Synchronising reads the file with the same choices again (see DataSetLoader::syncPackage), so an unchanged file changes nothing:
///the values read are compared with the values shown (see ImportColumn::valueLookupAsShown), in whichever language each of them is.
///A file that did change fills its columns again, and then every value has to find back the label it had (see Column::setValue).
void TestAll::testCsvSyncNumbers()
{
	QFETCH(QLocale,	interface);
	QFETCH(bool,	thousandSeparators);
	QFETCH(bool,	picked);
	QFETCH(QLocale,	readIn);

	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	const QString	csv = writeNumberSamples(dir);
	QVERIFY(!csv.isEmpty());

	auto interfaceIsSet	= interfaceLocale(interface, thousandSeparators);
	auto fileIsPicked	= csvPreviewPicked(picked ? std::optional<QLocale>(readIn) : std::nullopt);

	_importer->loadDataSet(fq(csv), [](int){});

	//MainWindow asks the user whether to synchronise, here the answer is always yes
	QMetaObject::Connection syncing = connect(_pkg, &DataSetPackage::checkDoSync, this, []{ return true; });
	auto stopSyncing = qScopeGuard([syncing]{ disconnect(syncing); });

	QSignalSpy changed(_pkg, &DataSetPackage::datasetChanged);

	auto synchronise = [&]
	{
		CSVImporter syncer(false);
		syncer.syncDataSet(fq(csv), [](int){});
	};

	auto samplesChanged = [&]
	{
		QStringList samples;
		for(const QString & column : changed.last()[0].toStringList()) //changedColumns, named by writeNumberSamples
			samples.push_back(QString::fromUtf8(NumbersInLocales::samples()[column.mid(QString("sample").size()).toULongLong()].written));
		return samples;
	};

	synchronise();

	QCOMPARE(changed.count(), 1);
	QVERIFY2(samplesChanged().isEmpty(), qPrintable("these samples count as changed: " + samplesChanged().join(", ")));

	QStringList	readWrong	= numberSamplesReadWrong(	_pkg->dataSet(), readIn),
				shownWrong	= numberSamplesShownWrong(	_pkg->dataSet(), readIn, interface, thousandSeparators);

	QVERIFY2(readWrong	.isEmpty(), qPrintable("\n" + readWrong	.join("\n")));
	QVERIFY2(shownWrong	.isEmpty(), qPrintable("\n" + shownWrong	.join("\n")));

	QVERIFY(!writeNumberSamples(dir, 2).isEmpty()); //One row more changes every column

	synchronise();

	QCOMPARE(changed.count(),		2);
	QCOMPARE(samplesChanged().size(),	int(NumbersInLocales::samples().size()));
	QCOMPARE(_pkg->dataSet()->rowCount(),	2);

	readWrong	= numberSamplesReadWrong(	_pkg->dataSet(), readIn);
	shownWrong	= numberSamplesShownWrong(	_pkg->dataSet(), readIn, interface, thousandSeparators);

	QVERIFY2(readWrong	.isEmpty(), qPrintable("\n" + readWrong	.join("\n")));
	QVERIFY2(shownWrong	.isEmpty(), qPrintable("\n" + shownWrong	.join("\n")));
}

///A sync compares numbers as numbers (however they are shown), but text as text: also text the interface would read as that same number,
///because the import did not. "1,234.56" is no German number, so in a German file it is text, even if JASP shows 1234.56 just like that in English.
void TestAll::testCsvSyncTextAndNumbersStayApart()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	QFile			csv(dir.filePath("german.csv"));

	auto writeCsv = [&](const QByteArray & content)
	{
		if(!csv.open(QIODevice::WriteOnly))
			return false;
		csv.write(content);
		csv.close();
		return true;
	};

	auto english	= interfaceLocale(QLocale(QLocale::English, QLocale::UnitedStates));
	auto german		= csvPreviewPicked(QLocale(QLocale::German, QLocale::Germany));

	QVERIFY(writeCsv("becomesNumber;becomesText\n1,234.56;1234,56\n"));
	_importer->loadDataSet(fq(csv.fileName()), [](int){});

	double number;
	QVERIFY(!_pkg->dataSet()->column("becomesNumber")	->numberAt(0, number));
	QVERIFY( _pkg->dataSet()->column("becomesText")		->numberAt(0, number));

	//The same number the other way around, and read the way the interface does, the file would be the same
	QVERIFY(writeCsv("becomesNumber;becomesText\n1234,56;1,234.56\n"));

	QMetaObject::Connection syncing = connect(_pkg, &DataSetPackage::checkDoSync, this, []{ return true; });
	auto stopSyncing = qScopeGuard([syncing]{ disconnect(syncing); });

	QSignalSpy changed(_pkg, &DataSetPackage::datasetChanged);

	CSVImporter syncer(false);
	syncer.syncDataSet(fq(csv.fileName()), [](int){});

	QCOMPARE(changed.count(), 1);

	QStringList changedColumns = changed[0][0].toStringList();
	changedColumns.sort();
	QCOMPARE(changedColumns, QStringList({ "becomesNumber", "becomesText" }));

	QVERIFY( _pkg->dataSet()->column("becomesNumber")	->numberAt(0, number));
	QCOMPARE(number, 1234.56);
	QVERIFY(!_pkg->dataSet()->column("becomesText")		->numberAt(0, number));
	QCOMPARE(_pkg->dataSet()->column("becomesText")->strs()[0], std::string("1,234.56")); //Still a scale column, which shows text as missing
}

///The delimiter and locale of a csv hold for a single load or sync, also one that fails: left behind, the next csv opened
///would not show its preview but be split with that delimiter (see DesktopCommunicator::askCsvDelimiter)
void TestAll::testFailedLoadOrSyncForgetsCsvChoices()
{
	if(_pkg) delete _pkg;

	_pkg = new DataSetPackage(this);
	_pkg->createDataSet();

	DesktopCommunicator * communicator = DesktopCommunicator::singleton();

	//An empty csv cannot be read (CSV::open throws), as happens when a synchronised file is being written just then
	QTemporaryDir	dir;
	QFile			empty(dir.filePath("empty.csv"));
	QVERIFY(empty.open(QIODevice::WriteOnly));
	empty.close();

	_pkg->dataSet()->setCsvChoices(';', "de");

	QVERIFY_THROWS_EXCEPTION(std::exception, DataSetLoader::syncPackage(fq(empty.fileName()), ".csv"));
	QCOMPARE(communicator->knownCsvDelimiter(), '\0');
	QVERIFY (!communicator->knownImportLocale());

	communicator->setKnownCsvDelimiter(';'); //Which is how data_load in MainWindow opens a csv without its preview

	QVERIFY_THROWS_EXCEPTION(std::exception, DataSetLoader::loadPackage(fq(empty.fileName()), ".csv"));
	QCOMPARE(communicator->knownCsvDelimiter(), '\0');
	QVERIFY (!communicator->knownImportLocale());
}

///The numbers in an .ods file are written the way C writes them (office:value), whatever the locale of the spreadsheet or of JASP,
///so an interface in whatever language must read them just like C does: a German one must not read 0.111 as one hundred and eleven.
void TestAll::testOdsImportLocale()
{
	QDir odsDir(_testLibrary());
	odsDir.cd("ods");

	auto importIt = [&]()
	{
		DatabaseInterface::closeInterfaces(); //Like cleanup() does, a DataSetPackage wants a database of its own
		delete _pkg;
		delete _importer;

		_pkg		= new DataSetPackage(this);
		_importer	= new ods::ODSImporter();

		_importer->loadDataSet(fq(odsDir.absoluteFilePath("Data Import Test ODS.ods")), [](int){});

		std::vector<doublevec> values;
		for(Column * column : _pkg->dataSet()->columns())
			values.push_back(column->dbls());

		return values;
	};

	const std::vector<doublevec> inC = importIt();

	for(const QLocale & interface : NumbersInLocales::locales())
		for(bool thousandSeparators : { false, true })
		{
			auto interfaceIsSet = interfaceLocale(interface, thousandSeparators);

			const std::vector<doublevec>	inInterface	= importIt();
			const QString					interfaceIs	= QLocale::languageToString(interface.language()) + (thousandSeparators ? " with thousand separators" : "");

			QCOMPARE(inInterface.size(), inC.size());

			for(size_t c=0; c<inC.size(); c++)
			{
				QCOMPARE(inInterface[c].size(), inC[c].size());

				for(size_t r=0; r<inC[c].size(); r++)
					QVERIFY2((std::isnan(inC[c][r]) && std::isnan(inInterface[c][r])) || inC[c][r] == inInterface[c][r], qPrintable(QString("column %1 row %2: %3 in C but %4 in %5").arg(c).arg(r).arg(inC[c][r]).arg(inInterface[c][r]).arg(interfaceIs)));
			}
		}
}

///Writes a Minitab worksheet (.mwx): a zip holding a metadata file and the sheet it points to, both json (see Minitab::findMetadataPath)
static bool writeMinitabWorksheet(const QString & path, const std::string & sheetJson)
{
	const std::map<std::string, std::string> files =
	{
		{ "sheet_metadata.json",	R"({ "Worksheet": { "Uri": "sheets/0/sheet" } })"	},
		{ "sheets/0/sheet.json",	sheetJson											}
	};

	struct archive * zip = archive_write_new();
	archive_write_set_format_zip(zip);

	bool written = archive_write_open_filename(zip, fq(path).c_str()) == ARCHIVE_OK;

	for(const auto & [name, content] : files)
	{
		struct archive_entry * entry = archive_entry_new();
		archive_entry_set_pathname(	entry, name.c_str());
		archive_entry_set_size(		entry, content.size());
		archive_entry_set_filetype(	entry, AE_IFREG);
		archive_entry_set_perm(		entry, 0644);

		written = written && archive_write_header(zip, entry) == ARCHIVE_OK && archive_write_data(zip, content.data(), content.size()) == la_ssize_t(content.size());

		archive_entry_free(entry);
	}

	written = archive_write_close(zip) == ARCHIVE_OK && written;
	archive_write_free(zip);

	return written;
}

///A Minitab worksheet stores numbers, not text written in some locale, so whatever the interface a number is read as the number it is:
///one with three decimals is what an interface grouping thousands with a dot would take for a thousand times as much (1.234 in German).
void TestAll::testMinitabImportLocale()
{
	QTemporaryDir	dir;
	const QString	worksheet	= dir.filePath("numbers.mwx");
	const doublevec	numbers		= { 1.234, 0.125, 12.375, 1234.5, 2 };

	QVERIFY(writeMinitabWorksheet(worksheet, R"({ "MaxRows_DEP": 5, "MaxColumns_DEP": 1, "Data": { "Columns": [
		{ "WorksheetVarBody": { "Name": "x", "VarData": { "VarDataBody": { "NumericData": [ 1.234, 0.125, 12.375, 1234.5, 2 ] } } } } ] } })"));

	for(const QLocale & interface : NumbersInLocales::locales())
		for(bool thousandSeparators : { false, true })
		{
			auto interfaceIsSet = interfaceLocale(interface, thousandSeparators);

			DatabaseInterface::closeInterfaces(); //Like cleanup() does, a DataSetPackage wants a database of its own
			delete _pkg;
			delete _importer;

			_pkg		= new DataSetPackage(this);
			_importer	= new MinitabImporter();

			_importer->loadDataSet(fq(worksheet), [](int){});

			Column * column = _pkg->dataSet() ? _pkg->dataSet()->column("x") : nullptr;
			QVERIFY(column);
			QCOMPARE(column->rowCount(), numbers.size());

			for(size_t row=0; row<numbers.size(); row++)
			{
				double read = 0;
				QVERIFY2(column->numberAt(row, read) && read == numbers[row], qPrintable(QString("%1 in row %2 is read as %3 by an interface in %4%5")
					.arg(numbers[row]).arg(row + 1).arg(read, 0, 'g', 12).arg(QLocale::languageToString(interface.language()), thousandSeparators ? " with thousand separators" : "")));
			}
		}
}

///A jaspfile saved before csvDelimiter and importLocale existed gets them when it is opened, also when it says it was saved by the version this is
void TestAll::testDataSetsTableUpgrade()
{
	if(_pkg)	delete _pkg;

	_pkg = new DataSetPackage(this);	//Gives a freshly created internal database holding one dataset

	DatabaseInterface * db = DatabaseInterface::singleton();

	db->runStatements("ALTER TABLE DataSets DROP COLUMN importLocale;");
	db->runStatements("ALTER TABLE DataSets DROP COLUMN csvDelimiter;");

	db->upgradeDBFromVersion(AppInfo::version);

	std::string	dataFilePath, description, databaseJson, emptyValuesJson, importLocale = "not read";
	long		dataFileTimestamp;
	int			revision;
	bool		dataSynch, showRSyntax;
	char		csvDelimiter = 'x';

	//Throws "no such column" when the upgrade left one of them out
	db->dataSetLoad(1, dataFilePath, dataFileTimestamp, description, databaseJson, emptyValuesJson, revision, dataSynch, showRSyntax, csvDelimiter, importLocale);

	QCOMPARE(csvDelimiter,	'\0');
	QCOMPARE(importLocale,	std::string(""));
}

// Regression test for https://github.com/jasp-stats/jasp-desktop/commit/0a90b9a34e9d754f55bc32ec1efd2f67940ef756
// setDataSetSize() pre-allocates rows before initFromLookups() is called, causing rowCount() > 0
// when setValues() checks allTheSame — which skipped the label-detection loop and silently dropped
// all SPSS value labels.
void TestAll::testSavLabels()
{
	if(_pkg)	delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new ReadStatImporter();

	const QString savPath = _testLibrary().absoluteFilePath("readstat/Labelled_data.sav");
	_importer->loadDataSet(fq(savPath), [](int){});

	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet, "No dataset!");

	// These columns have SPSS value labels (e.g. 1->"Soha", 2->"Havonta vagy kevesebbszer", …)
	// and must be imported as labelled (nominal/ordinal) columns.
	const QStringList labelledColumns = {
		"AUDIT_gyakorisag",
		"AUDIT_mennyiség",
		"PHQ14_fejfajas",
		"PHQ14_szivveres",
		"PHQ9_energia"
	};

	for(const QString & colName : labelledColumns)
	{
		Column * col = dataSet->column(fq(colName));
		QVERIFY2(col,				qPrintable("Column not found: "	+ colName));
		QVERIFY2(col->hasLabels(),			qPrintable("Column has no labels: "  + colName));
		QVERIFY2(col->labels().size() > 0,	qPrintable("Label list is empty: "   + colName));
	}

	// Spot-check: AUDIT_gyakorisag label 1 should be "Soha"
	Column * audit = dataSet->column("AUDIT_gyakorisag");
	QVERIFY2(audit, "AUDIT_gyakorisag column not found");

	bool foundSoha = false;
	for(const Label * label : audit->labels())
		if(label->labelDisplay() == "Soha") { foundSoha = true; break; }

	QVERIFY2(foundSoha, "Expected label 'Soha' not found in AUDIT_gyakorisag");

	// Scale columns must NOT have labels
	const QStringList scaleColumns = { "Eletkor", "MHC_SF_Emo", "PSS_10" };
	for(const QString & colName : scaleColumns)
	{
		Column * col = dataSet->column(fq(colName));
		QVERIFY2(col, qPrintable("Column not found: " + colName));
		QVERIFY2(!col->hasLabels(), qPrintable("Scale column should not have labels: " + colName));
	}
}

// Regression test for https://github.com/jasp-stats/jasp-issues/issues/4293
void TestAll::testFilterLabels()
{
	if(_pkg)	delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new ReadStatImporter();

	const QString filePath = _testLibrary().absoluteFilePath("jasp/Directed Reading Activities.jasp");
	JASPImporter::loadDataSet(fq(filePath),		[](int){});

	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet, "No dataset!");

	std::string colName = "group";
	Column * col = dataSet->column(colName);
	QVERIFY2(col,										qPrintable("Group Column not found"));
	QVERIFY2(col->hasLabels(),							qPrintable("Group has no labels"));
	QVERIFY2(col->labelsNonEmptyCount() == 2,			qPrintable(tq("Number of labels is not 2: ")) + col->labelsNonEmptyCount());

	Label * controlLabel = col->labelByIndexNonEmpty(0);
	Label * treatLabel = col->labelByIndexNonEmpty(1);
	QVERIFY2(controlLabel->label() == "Control",		qPrintable("First label is not 'Control'"));
	QVERIFY2(controlLabel->filterAllows(),				qPrintable("'Control' label is filtered"));
	QVERIFY2(treatLabel->label() == "Treat",			qPrintable("Second label is not 'Treat'"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Do as if the user clicked on Filter for the Control label in the Label window
	_pkg->setData(_pkg->indexForSubNode(controlLabel), false, int(DataSetPackage::specialRoles::filter));
	QVERIFY2(!controlLabel->filterAllows(),				qPrintable("'Control' label is not filtered"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Not all labels can be unset: nothing should change
	_pkg->setData(_pkg->indexForSubNode(treatLabel), false, int(DataSetPackage::specialRoles::filter));
	QVERIFY2(!controlLabel->filterAllows(),				qPrintable("'Control' label is not filtered"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Set first the Control label, and unset the Treat lable: this time it should work
	_pkg->setData(_pkg->indexForSubNode(controlLabel), true, int(DataSetPackage::specialRoles::filter));
	_pkg->setData(_pkg->indexForSubNode(treatLabel), false, int(DataSetPackage::specialRoles::filter));
	QVERIFY2(controlLabel->filterAllows(),				qPrintable("'Control' label is filtered"));
	QVERIFY2(!treatLabel->filterAllows(),				qPrintable("'Treat'label is not filtered"));

}


void TestAll::testSyncKeepMissingColumns()
{
	_pkg = new DataSetPackage(this);

	//Importer::syncDataSet reads the preferences singleton; nothing else in the tests creates one.
	if(!PreferencesModel::prefs())
		new PreferencesModel(this);
	QVERIFY(PreferencesModel::prefs());


	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	auto writeCsv = [&](const QString & name, const QByteArray & content)
	{
		QString path = tempDir.filePath(name);
		QFile	file(path);
		if(!file.open(QIODevice::WriteOnly))
			return QString();
		file.write(content);
		file.close();
		return path;
	};

	const QString	abc	= writeCsv("abc.csv",	"a,b,c\n1,2,3\n"),
					ab	= writeCsv("ab.csv",	"a,b\n4,5\n"),
					pq	= writeCsv("pq.csv",	"p,q\n6,7\n");

	QVERIFY(!abc.isEmpty() && !ab.isEmpty() && !pq.isEmpty());

	auto freshDataSetFrom = [&](const QString & csv)
	{
		_pkg->createDataSet();

		CSVImporter importer;
		importer.loadDataSet(fq(csv), [](int){});
		return DataSetPackage::pkg()->dataSet();
	};

	//Without the preference a column that is gone from the new data file is removed.
	{
		PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(false);

		DataSet * ds = freshDataSetFrom(abc);
		QCOMPARE(ds->columnCount(), 3);

		CSVImporter syncer;
		syncer.syncDataSet(fq(ab), [](int){});

		QCOMPARE(ds->columnCount(), 2);
		QVERIFY(!ds->column("c"));
	}

	//With the preference that same column is kept instead of removed.
	{
		PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(true);

		DataSet * ds = freshDataSetFrom(abc);
		QCOMPARE(ds->columnCount(), 3);

		CSVImporter syncer;
		syncer.syncDataSet(fq(ab), [](int){});

		QCOMPARE(ds->columnCount(), 3);
		QVERIFY(ds->column("c"));

		//Syncing on against a file that shares no column at all keeps every one of them: p and q are added
		//next to a, b and c rather than taking their place, so the data holds the union of both files. That
		//union keeps growing for as long as one session goes on synchronizing, which is why each data file
		//normally gets a JASP process of its own.
		CSVImporter syncer2;
		syncer2.syncDataSet(fq(pq), [](int){});

		QCOMPARE(ds->columnCount(), 5);
		QVERIFY(ds->column("a"));
		QVERIFY(ds->column("b"));
		QVERIFY(ds->column("c"));
		QVERIFY(ds->column("p"));
		QVERIFY(ds->column("q"));
	}

	PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(false);

	//The singleton registers globally (PreferencesModelBase::_singleton) and is parented to this
	//test, so it would survive this test and then make MainWindow's own PreferencesModel assert.
	delete PreferencesModel::prefs();
}

///A script run from JASP calls the same methods as the AI, but it is the user's own code, so its calls leave the AI's view of the workspace alone
///(see RpcCaller): otherwise the AI would never hear what a script read or changed, and a script would get the AI's divergence error.
void TestAll::testRpcScriptCallerLeavesTheAgentViewAlone()
{
	JaspRpcDispatcher dispatcher;

	//A method reading the workspace and one changing it (x-failOnStateDiverged), both marking what they show as observed, like data_info and get_analyses_state do
	QCOMPARE(dispatcher.loadSpecFromString(R"({"openrpc":"1.2.6","methods":[
		{"name":"test_read",	"params":[]},
		{"name":"test_change",	"params":[],	"x-failOnStateDiverged":true} ]})"), 2);

	const size_t	analysisId		= 1;
	RpcCaller		handlerSaw		= RpcCaller::Ai;
	bool			scriptWasSeen	= false;

	auto handler = [&](const Json::Value &)
	{
		handlerSaw		= dispatcher.currentCaller();
		scriptWasSeen	= JaspRpcDispatcher::scriptIsCalling();
		AgentStateTracker::notifyDataObserved();
		AgentStateTracker::notifyAnalysisObserved(analysisId);
		return JaspRpcDispatcher::successResult();
	};

	QVERIFY(dispatcher.registerMethodByName("test_read",	handler));
	QVERIFY(dispatcher.registerMethodByName("test_change",	handler));

	auto call = [&](const std::string & method, RpcCaller caller)
	{
		Json::Value request;
		request["jsonrpc"]	= "2.0";
		request["id"]		= 1;
		request["method"]	= method;
		return dispatcher.dispatch(request, caller);
	};

	AgentStateTracker::init();
	AgentStateTracker * tracker = AgentStateTracker::tracker();
	QVERIFY(tracker);
	auto forgetChanges = qScopeGuard([tracker]{ tracker->clearAll(); }); //The tracker is a singleton that outlives this test

	tracker->clearAll();

	//The user added a column, which the AI has not seen yet: a script reading the data leaves it unseen
	tracker->markDataChanged({ "added" }, {}, {}, {});

	Json::Value reply = call("test_read", RpcCaller::Script);
	QVERIFY (handlerSaw == RpcCaller::Script && scriptWasSeen);
	QVERIFY2(tracker->isDirty(),							"what a script reads of the data, the AI still has to see");

	//The user changed an option: a script reading the analysis gets no state update, and leaves it unseen as well
	tracker->clearAll();
	tracker->markAnalysisOptionsChanged(analysisId);

	reply = call("test_read", RpcCaller::Script);
	QVERIFY2(!reply["result"].isMember("_stateUpdate"),	"a script should not get the AI's state update");
	QVERIFY2(tracker->isDirty(),							"what a script reads of an analysis, the AI still has to see");

	//Where the AI's change would fail, a script's goes ahead and leaves the change for the AI to see
	reply = call("test_change", RpcCaller::Script);
	QVERIFY2(reply.isMember("result"),						"a script's change should not fail because the AI has not seen the workspace");
	QVERIFY2(tracker->isUserDiverged(),						"a script's change should not clear what the AI has not seen");

	QVERIFY(dispatcher.currentCaller() == RpcCaller::Ai && !JaspRpcDispatcher::scriptIsCalling()); //Only while a script's call runs

	//The AI still learns the workspace changed under it
	reply = call("test_change", RpcCaller::Ai);
	QCOMPARE(reply["error"]["code"].asInt(), -32001); //Refused before its handler runs
	QVERIFY (!tracker->isDirty());

	tracker->markAnalysisOptionsChanged(analysisId + 1); //Another analysis than the handler observes, so it is left for _stateUpdate

	reply = call("test_read", RpcCaller::Ai);
	QVERIFY (handlerSaw == RpcCaller::Ai && !scriptWasSeen);
	QVERIFY2(reply["result"].isMember("_stateUpdate"),		"the AI should get what changed");
	QVERIFY (!tracker->isDirty());
}

///A call made while another waits for R in a nested event loop sits on top of that call's stack, so it cannot wait there:
///dispatch() refuses it (-32000), dispatchWhenFree() runs it once the dispatcher is free, in the order the calls came in.
void TestAll::testRpcDispatchWhenFreeWaitsItsTurn()
{
	JaspRpcDispatcher dispatcher;

	QStringList ran, answered;

	dispatcher.registerMethod("test_record", [&](const Json::Value & params)
	{
		ran.push_back(tq(params["name"].asString()) + (JaspRpcDispatcher::scriptIsCalling() ? " by a script" : " by the AI"));
		return JaspRpcDispatcher::successResult();
	});

	auto request = [](const std::string & method, const std::string & name = "")
	{
		return R"({"jsonrpc":"2.0","id":1,"method":")" + method + R"(","params":{"name":")" + name + R"("}})";
	};

	auto answer = [&](const QString & name)
	{
		return [&answered, name](const std::string & response)
		{
			Json::Value parsed;
			Json::Reader().parse(response, parsed);
			answered.push_back(name + (parsed.isMember("result") ? "" : " refused"));
		};
	};

	dispatcher.dispatchWhenFree(request("test_record", "free"), RpcCaller::Script, answer("free"));
	QCOMPARE(answered, QStringList({ "free" })); //Right away, when nothing is in flight

	size_t		waitingDuringWait	= 0;
	Json::Value	refused;

	//Like analysis_run while R runs: calls come in during the nested event loop
	dispatcher.registerMethod("test_wait", [&](const Json::Value &)
	{
		dispatcher.dispatchWhenFree(request("test_record", "first"),	RpcCaller::Script,	answer("first"));
		dispatcher.dispatchWhenFree(request("test_record", "second"),	RpcCaller::Ai,		answer("second"));
		Json::Reader().parse(dispatcher.dispatch(request("test_record", "direct")), refused);

		JaspRpcDispatcher::waitAndProcessEvents(50, [](QEventLoop &, QTimer &){});
		waitingDuringWait = dispatcher.waitingCount();

		return JaspRpcDispatcher::successResult();
	});

	dispatcher.dispatch(request("test_wait"));

	QCOMPARE(waitingDuringWait,						size_t(2));	//Events ran, but the calls stayed queued on top of the one in flight
	QCOMPARE(refused["error"]["code"].asInt(),		-32000);
	QCOMPARE(ran,									QStringList({ "free by a script" }));

	QTRY_COMPARE(answered,	QStringList({ "free", "first", "second" }));	//From the event loop, once the call in flight is done
	QCOMPARE(ran,			QStringList({ "free by a script", "first by a script", "second by the AI" }));
	QCOMPARE(dispatcher.waitingCount(), size_t(0));
}

///The HTTP server answers a call made while another waits for R once its turn comes, instead of refusing it with -32000
void TestAll::testRpcServerQueuesBusyCalls()
{
	JaspRpcDispatcher	dispatcher;
	JaspRpcServer		server(dispatcher, nullptr, "127.0.0.1", 0);
	QVERIFY(server.start());

	QNetworkAccessManager network;
	network.setProxy(QNetworkProxy::NoProxy);

	auto post = [&](const QByteArray & body)
	{
		QNetworkRequest request(QUrl(QString("http://127.0.0.1:%1/rpc").arg(server.serverPort())));
		request.setHeader(QNetworkRequest::ContentTypeHeader, "application/json");
		return network.post(request, body);
	};

	size_t							waitingSeen = 0;
	std::unique_ptr<QNetworkReply>	queued;

	//Sends another request and waits, like analysis_run does for R, until that one has come in and is waiting its turn
	dispatcher.registerMethod("test_wait", [&](const Json::Value &)
	{
		queued.reset(post(R"({"jsonrpc":"2.0","id":2,"method":"ping"})"));

		JaspRpcDispatcher::waitAndProcessEvents(10000, [&](QEventLoop & loop, QTimer &)
		{
			QTimer * poll = new QTimer(&loop);
			QObject::connect(poll, &QTimer::timeout, &loop, [&]
			{
				if ((waitingSeen = dispatcher.waitingCount()) > 0)
					loop.quit();
			});
			poll->start(5);
		});

		return JaspRpcDispatcher::successResult();
	});

	std::unique_ptr<QNetworkReply> waiting(post(R"({"jsonrpc":"2.0","id":1,"method":"test_wait"})"));

	QTRY_VERIFY_WITH_TIMEOUT(waiting->isFinished() && queued && queued->isFinished(), 15000);
	QCOMPARE(waitingSeen, size_t(1));

	Json::Value waitingAnswer, queuedAnswer;
	Json::Reader().parse(waiting	->readAll().toStdString(), waitingAnswer);
	Json::Reader().parse(queued		->readAll().toStdString(), queuedAnswer);

	QCOMPARE(waitingAnswer	["result"]["status"]	.asString(), std::string("success"));
	QCOMPARE(queuedAnswer	["result"]["message"]	.asString(), std::string("pong"));
}


QTEST_MAIN(TestAll)
