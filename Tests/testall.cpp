#include "testall.h"
#include "testinfo.h"
#include "numbersinlocales.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "qutils.h"
#include "databaseinterface.h"
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"
#include "data/importers/odsimporter.h"
#include "data/importers/jaspimporter.h"
#include "data/exporters/jaspexporter.h"
#include "data/exporters/dataexporter.h"
#include "data/importers/excelimporter.h"
#include "data/importers/rdataimporter.h"
#include "data/importers/readstatimporter.h"
#include "data/importers/minitabimporter.h"
#include "data/importers/database/databaseimportcolumn.h"
#include "utilities/settings.h"
#include "gui/preferencesmodel.h"
#include "utilities/desktopcommunicator.h"
#include "datasetsyncer.h"
#include <QTemporaryDir>
#include <QScopeGuard>
#include "columnutils.h"
#include "appinfo.h"
#include <cmath>
#include "dataset.h"
#include "workspace.h"
#include "undostack.h"
#include "data/asyncloader.h"
#include "data/importers/csv/csvparser.h"
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
	//The CSV delimiter scratchpad (_knownCsvDelimiter) is a per-import value in production
	//(reset by DataSetLoader); make sure a leftover value can never leak between tests.
	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');
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

bool TestAll::_newPkgWithDataSet()
{
	delete _importer;
	_importer = nullptr;
	delete _pkg;
	_pkg = nullptr;

	_pkg = new DataSetPackage(this);

	//Reset the per-import CSV delimiter scratchpad so it can't leak from a previous import.
	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');

	CSVImporter importer;
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), _pkg->createDataSet(), [](int){});

	return _pkg->dataSet() != nullptr;
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
		if(folder == "csv")			return new CSVImporter();

		return nullptr;
	};

	if(_pkg)
		delete _pkg;

	if(_importer)
		delete _importer;

	_pkg = new DataSetPackage(this);
	_importer = getImporter();

	QVERIFY2(_importer, "Getting importer failed...");

	//Reset the per-import CSV delimiter scratchpad so one file's delimiter can't leak into the next.
	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');

	std::cerr << "Testing " << dataFileAbsolutePath << std::endl;
	_importer->loadDataSet(fq(dataFileAbsolutePath), _pkg->createDataSet(), [](int i){});

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

	
	DataSet loadMe(nullptr, dataSet->id());
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

	
	DataSet loadMe(nullptr, dataSet->id());
	QVERIFY2(dataSet->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

///Cancelling the csv preview aborts the import from inside loadDataSet. A model reset that is left open then makes the whole of JASP draw
///nothing at all, which is what a blank window after Cancel comes down to. And nothing of the aborted import should be kept.
void TestAll::testCancelledImportLeavesTheModelUsable()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(true);	//true: this one asks for a delimiter, so it can be cancelled

	DataSet * dataSet = _pkg->createDataSet(); //As DataSetLoader::loadPackage does before it imports

	//The workspace and the dataset it shows are the models the data view draws
	QSignalSpy workspaceResetStarted(	_pkg->workspace(),	&QAbstractItemModel::modelAboutToBeReset);
	QSignalSpy workspaceResetFinished(	_pkg->workspace(),	&QAbstractItemModel::modelReset);
	QSignalSpy dataSetResetStarted(		dataSet,			&QAbstractItemModel::modelAboutToBeReset);
	QSignalSpy dataSetResetFinished(	dataSet,			&QAbstractItemModel::modelReset);

	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0'); //Otherwise it would not ask at all

	//Answering with '\0' is what pressing Cancel in the preview window comes down to
	QMetaObject::Connection cancelling = connect(DesktopCommunicator::singleton(), &DesktopCommunicator::askCsvDelimiterSignal,
		[](const QString &, char) { DesktopCommunicator::singleton()->delimiterChosen('\0'); });

	QDir csvDir(_testLibrary());
	csvDir.cd("csv");

	bool threw = false;

	try
	{
		_importer->loadDataSet(fq(csvDir.absoluteFilePath("Data Import Test CSV.csv")), dataSet, [](int){});
	}
	catch(const std::exception &)
	{
		threw = true; //"No data loaded", the way an aborted import always ends
	}

	disconnect(cancelling);

	QVERIFY2(threw, "cancelling the preview should abort the import");
	QCOMPARE(workspaceResetFinished.count(),	workspaceResetStarted.count());	//Every begin got its end, so the models are usable again
	QCOMPARE(dataSetResetFinished.count(),		dataSetResetStarted.count());
	QCOMPARE(dataSet->columnCount(),			0);								//Nothing of the aborted import is kept, so the next import takes this empty dataset (see Workspace::createDataSet)
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

	_importer->loadDataSet(fq(csv.fileName()), _pkg->createDataSet(), [](int){});

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

	_importer->loadDataSet(fq(csv.fileName()), _pkg->createDataSet(), [](int){});

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

	_importer->loadDataSet(fq(csv), _pkg->createDataSet(), [](int){});

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

	_importer->loadDataSet(fq(csv), _pkg->createDataSet(), [](int){});

	//MainWindow asks the user whether to synchronise, here the answer is always yes
	QMetaObject::Connection syncing = connect(_pkg, &DataSetPackage::checkDoSync, this, []{ return true; });
	auto stopSyncing = qScopeGuard([syncing]{ disconnect(syncing); });

	QSignalSpy changed(_pkg, &DataSetPackage::datasetChanged);

	auto synchronise = [&]
	{
		CSVImporter syncer(false);
		syncer.syncDataSet(fq(csv), _pkg->dataSet(), [](int){});
	};

	auto samplesChanged = [&]
	{
		QStringList samples;
		for(const QString & column : changed.last()[1].toStringList()) //changedColumns (after dataSetID), named by writeNumberSamples
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
	_importer->loadDataSet(fq(csv.fileName()), _pkg->createDataSet(), [](int){});

	double number;
	QVERIFY(!_pkg->dataSet()->column("becomesNumber")	->numberAt(0, number));
	QVERIFY( _pkg->dataSet()->column("becomesText")		->numberAt(0, number));

	//The same number the other way around, and read the way the interface does, the file would be the same
	QVERIFY(writeCsv("becomesNumber;becomesText\n1234,56;1,234.56\n"));

	QMetaObject::Connection syncing = connect(_pkg, &DataSetPackage::checkDoSync, this, []{ return true; });
	auto stopSyncing = qScopeGuard([syncing]{ disconnect(syncing); });

	QSignalSpy changed(_pkg, &DataSetPackage::datasetChanged);

	CSVImporter syncer(false);
	syncer.syncDataSet(fq(csv.fileName()), _pkg->dataSet(), [](int){});

	QCOMPARE(changed.count(), 1);

	QStringList changedColumns = changed[0][1].toStringList(); //After dataSetID
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

	QVERIFY_THROWS_EXCEPTION(std::exception, DataSetLoader::syncPackage(fq(empty.fileName()), ".csv", _pkg->dataSet()));
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

		_importer->loadDataSet(fq(odsDir.absoluteFilePath("Data Import Test ODS.ods")), _pkg->createDataSet(), [](int){});

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

			_importer->loadDataSet(fq(worksheet), _pkg->createDataSet(), [](int){});

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

	_pkg = new DataSetPackage(this);	//Gives a freshly created internal database

	const int dataSetId = _pkg->createDataSet()->id();

	DatabaseInterface * db = DatabaseInterface::singleton();

	db->runStatements("ALTER TABLE DataSets DROP COLUMN importLocale;");
	db->runStatements("ALTER TABLE DataSets DROP COLUMN csvDelimiter;");

	db->upgradeDBFromVersion(AppInfo::version);

	std::string	title, dataFilePath, description, databaseJson, emptyValuesJson, importLocale = "not read";
	long		dataFileTimestamp;
	int			revision;
	bool		dataSynch;
	char		csvDelimiter = 'x';

	//Throws "no such column" when the upgrade left one of them out
	db->dataSetLoad(dataSetId, title, dataFilePath, dataFileTimestamp, description, databaseJson, emptyValuesJson, revision, dataSynch, csvDelimiter, importLocale);

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
	_importer->loadDataSet(fq(savPath), _pkg->createDataSet(), [](int){});

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
	col->setLabelAllowFilter(0, false);
	QVERIFY2(!controlLabel->filterAllows(),				qPrintable("'Control' label is not filtered"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Not all labels can be unset: nothing should change
	col->setLabelAllowFilter(1, false);
	QVERIFY2(!controlLabel->filterAllows(),				qPrintable("'Control' label is not filtered"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Set first the Control label, and unset the Treat label: this time it should work
	col->setLabelAllowFilter(0, true);
	col->setLabelAllowFilter(1, false);
	QVERIFY2(controlLabel->filterAllows(),				qPrintable("'Control' label is filtered"));
	QVERIFY2(!treatLabel->filterAllows(),				qPrintable("'Treat'label is not filtered"));
}

void TestAll::testCsvParserTrimsOnlyUnquotedPadding()
{
	CSVParser parser(',');

	const CSVParser::Grid grid = parser.parse(std::string(
		" a , b\t,\tc \n"
		"\" a \",\" b\",\"c \"\n"
		" \"a\" ,\t\"b\"\t,\"c\"\n"
		"   ,\"\",\"  \"\n"
		"\"a,b\",\"a\"\"b\",d\n"));

	QCOMPARE(grid.size(), size_t(5));

	//Unquoted padding is not data, so it goes.
	QCOMPARE(grid[0], stringvec({"a", "b", "c"}));

	//What sits inside the quotes is data, so it stays.
	QCOMPARE(grid[1], stringvec({" a ", " b", "c "}));

	//And padding around a quoted field is padding too.
	QCOMPARE(grid[2], stringvec({"a", "b", "c"}));

	//A field of nothing but whitespace is empty (which the importers read as a missing value), while
	//quoted whitespace is a value of its own.
	QCOMPARE(grid[3], stringvec({"", "", "  "}));

	//None of this may disturb the quoting itself.
	QCOMPARE(grid[4], stringvec({"a,b", "a\"b", "d"}));
}

// ---------- DataSetSyncer tests ----------

void TestAll::testSyncerStartStopFileSyncing()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();
	QVERIFY(!syncer.isFileSyncing());

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("test_startstop.csv");
	QFile f(testFilePath);
	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("a,b,c\n1,2,3\n");
	f.close();

	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), testFilePath);
	QVERIFY(ds->dataFileSynch());

	syncer.stopFileSyncing();
	QVERIFY(!syncer.isFileSyncing());
	QVERIFY(!ds->dataFileSynch());
}

void TestAll::testSyncerFileChangeEmitsSignal()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("sync_emit.csv");
	QFile f(testFilePath);
	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("x,y\n1,2\n");
	f.close();

	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());

	QTest::qSleep(1200);

	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("x,y\n3,4\n");
	f.close();

	// The file watcher signal is async; we check via syncRequired spy.
	// Signal args: (int dataSetId, DataSet * dataSet, QString locator, QString extension, QString databaseJson).
	QSignalSpy spy(&syncer, &DataSetSyncer::syncRequired);
	QTRY_COMPARE_WITH_TIMEOUT(spy.count(), 1, 5000);

	QList<QVariant> args = spy.takeFirst();
	QCOMPARE(args.size(), 4); //(DataSet*, locator, extension, databaseJson)
	QCOMPARE(args[1].toString(), testFilePath);
	QCOMPARE(args[2].toString(), QString("csv")); //extension
	QVERIFY(args[3].toString().isEmpty()); //databaseJson

	syncer.stopFileSyncing();
}

void TestAll::testSyncerStartStopDatabaseSyncing()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QVERIFY(!syncer.isDatabaseSyncing());
	QVERIFY(!ds->isDatabase());

	Json::Value dbJson;
	dbJson["dbType"] = "NOTCHOSEN";
	dbJson["interval"] = 1;

	syncer.startDatabaseSyncing(dbJson, false);
	QVERIFY(syncer.isDatabaseSyncing());
	QVERIFY(ds->isDatabase());
	QVERIFY(syncer.databaseJson() != Json::nullValue);

	syncer.stopDatabaseSyncing();
	QVERIFY(!syncer.isDatabaseSyncing());
	QVERIFY(!ds->isDatabase());
}

void TestAll::testSyncerSyncNowWithoutDataSource()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QSignalSpy spy(&syncer, &DataSetSyncer::askUserForRelink);

	syncer.syncNow();

	QTRY_COMPARE_WITH_TIMEOUT(spy.count(), 1, 1000);
	QCOMPARE(spy.takeFirst()[0].toInt(), ds->id());
}

void TestAll::testSyncerMultipleStartStop()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString path1 = tempDir.filePath("multi1.csv");
	QString path2 = tempDir.filePath("multi2.csv");

	auto makeFile = [&](const QString & p)
	{
		QFile f(p);
		QVERIFY(f.open(QIODevice::WriteOnly));
		f.write("a\n1\n");
		f.close();
	};

	makeFile(path1);
	makeFile(path2);

	syncer.startFileSyncing(path1);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), path1);

	syncer.startFileSyncing(path2);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), path2);

	syncer.stopFileSyncing();
	QVERIFY(!syncer.isFileSyncing());

	syncer.startFileSyncing(path1);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), path1);

	syncer.stopFileSyncing();
}


void TestAll::testDataExporterShownDataSetOnly()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString csvPath = tempDir.filePath("export.csv");

	// Import debug.csv — this creates the first dataset
	DataSet * firstDs = nullptr;
	{
		CSVImporter importer;
		importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), _pkg->createDataSet(), [](int){});
		firstDs = _pkg->dataSet();
		QVERIFY(firstDs);
		QVERIFY(firstDs->rowCount() > 0);
		QVERIFY(firstDs->columnCount() > 0);
	}

	// Create a second, empty dataset and make it the shown one
	DataSet * secondDs = _pkg->createDataSet();
	QVERIFY(secondDs);
	_pkg->workspace()->setShownDataSet(secondDs);
	secondDs = _pkg->dataSet();
	QVERIFY(secondDs);
	QVERIFY(secondDs != firstDs);

	secondDs->setColumnCount(1);
	secondDs->setRowCount(1, false);
	secondDs->column(0)->setName("mycol");
	secondDs->column(0)->setDefaultValues(columnType::scale, false);
	QCOMPARE(secondDs->rowCount(), 1);
	QCOMPARE(secondDs->columnCount(), 1);

	// Set a value manually
	QModelIndex idx = secondDs->index(0, 0);
	secondDs->setData(idx, "testval", Qt::DisplayRole);

	// Export using DataExporter — should export the shownDataSet only
	DataExporter exporter(false);
	exporter.saveDataSet(fq(csvPath), [](int){});

	// Read back and verify
	QFile csvFile(csvPath);
	QVERIFY(csvFile.open(QIODevice::ReadOnly));
	QString content = QString::fromUtf8(csvFile.readAll());
	csvFile.close();

	QStringList lines = content.split('\n', Qt::SkipEmptyParts);

	// Only the shown dataset (mycol) should be written
	QCOMPARE(lines.size(), 2); // header + 1 data row
	QVERIFY(lines[1].contains("testval"));

	// Verify that debug.csv columns are NOT present
	QVERIFY(!lines[0].contains("contNormal"));
	QVERIFY(!lines[0].contains("contGamma"));
}


void TestAll::testSyncerExportModifyReimport()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	// Create an initial CSV
	QString srcPath = tempDir.filePath("source.csv");
	QFile src(srcPath);
	QVERIFY(src.open(QIODevice::WriteOnly));
	src.write("a,b,c\n1,2,3\n4,5,6\n");
	src.close();

	// Import it
	CSVImporter importer;
	importer.loadDataSet(fq(srcPath), _pkg->createDataSet(), [](int){});
	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QCOMPARE(ds->rowCount(), 2);
	QCOMPARE(ds->columnCount(), 3);

	// Export to a new location
	QString exportPath = tempDir.filePath("exported.csv");
	DataExporter exporter(false);
	exporter.saveDataSet(fq(exportPath), [](int){});

	// Verify the exported file matches the original content
	QFile exported(exportPath);
	QVERIFY(exported.open(QIODevice::ReadOnly));
	QString exportedContent = QString::fromUtf8(exported.readAll());
	exported.close();

	QVERIFY(exportedContent.contains("a,b,c"));
	QVERIFY(exportedContent.contains("1,2,3"));
	QVERIFY(exportedContent.contains("4,5,6"));
}

void TestAll::testSyncerReleasesSyncGuardOnCompletion()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	// A DB-backed dataset that wants to sync. The re-entrancy guard must be released on *completion*
	// (setSyncingResult), so a subsequent sync can run. If the guard were never released, every
	// later sync would be swallowed — the exact wedge this refactor fixes.
	Json::Value dbJson;
	dbJson["dbType"] = "NOTCHOSEN";
	dbJson["interval"] = 1;

	syncer.startDatabaseSyncing(dbJson, false);
	QVERIFY(syncer.isDatabaseSyncing());

	QSignalSpy startedSpy(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy(&syncer, &DataSetSyncer::syncingFinished);

	// Trigger sync #1 and complete it.
	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy.count(), 1, 1000);
	syncer.setSyncingResult(true);
	QCOMPARE(finishedSpy.count(), 1);

	// Trigger sync #2; because the guard was released, it must start again rather than early-return.
	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy.count(), 2, 1000);
	syncer.setSyncingResult(false);
	QCOMPARE(finishedSpy.count(), 2);

	syncer.stopDatabaseSyncing();
}

void TestAll::testSyncerRetriesFileChangeMissedDuringSync()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("sync_retry.csv");
	QFile f(testFilePath);
	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("x,y\n1,2\n");
	f.close();

	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());

	QSignalSpy startedSpy(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy syncRequiredSpy(&syncer, &DataSetSyncer::syncRequired);

	//Deterministic trigger helper: instead of waiting for the OS file watcher (and second-resolution
	//mtimes) we invoke the same slot the watcher is connected to, after resetting the stored timestamp
	//so the mtime filter inside fileChanged always passes. The watcher/mtime path is covered end-to-end
	//by testSyncerFileChangeEmitsSignal; here we only exercise the missed-change replay logic.
	auto changeFile = [&](const char * contents)
	{
		QVERIFY(f.open(QIODevice::WriteOnly));
		f.write(contents);
		f.close();
		ds->setDataFileAndTimeStamp(fq(testFilePath), 0);
		QVERIFY(QMetaObject::invokeMethod(&syncer, "fileChanged", Q_ARG(QString, testFilePath)));
	};

	// 1) Trigger a sync that stays in-flight (the guard is held until setSyncingResult).
	changeFile("x,y\n3,4\n");
	QCOMPARE(startedSpy.count(), 1);
	QCOMPARE(syncRequiredSpy.count(), 1);

	// 2) A change arrives while the sync is still in-flight: it must be remembered, not dropped, and
	//    must NOT start a new sync yet (the guard is still held).
	changeFile("x,y\n5,6\n");
	QCOMPARE(startedSpy.count(), 1);

	// 3) Completing the in-flight sync must release the guard AND replay the missed change.
	syncer.setSyncingResult(true);
	QCOMPARE(startedSpy.count(), 2);
	QCOMPARE(syncRequiredSpy.count(), 2);

	syncer.stopFileSyncing();
}

void TestAll::testFilterSetFilterVectorResizesToResult()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QVERIFY(ds->rowCount() > 0);

	Filter * filter = ds->defaultFilter();
	QVERIFY(filter);

	const size_t originalRows = static_cast<size_t>(ds->rowCount());

	//Seed a cache matching the current dataset.
	boolvec initial(originalRows, true);
	initial[0] = false;
	filter->setFilterVector(initial);
	QCOMPARE(filter->filtered().size(), originalRows);

	//The dataset grew: the engine result is authoritative and must be adopted in full (new rows at
	//the end get the engine's value), instead of silently dropping everything past the old size.
	boolvec bigger(originalRows + 3, false);
	bigger[0] = false, bigger[1] = true, bigger[bigger.size() - 1] = true;
	filter->setFilterVector(bigger);
	QCOMPARE(filter->filtered().size(), originalRows + 3);
	QVERIFY(filter->filtered() == bigger);

	//And when the result shrinks, stale tail rows must not survive.
	boolvec smaller(originalRows - 2, true);
	filter->setFilterVector(smaller);
	QCOMPARE(filter->filtered().size(), originalRows - 2);
	QVERIFY(filter->filtered() == smaller);
}

void TestAll::testComputedDataSetCycleDetection()
{
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);

	//Workspace::createDataSet reuses the currently-shown (empty) dataset, so make each one
	//non-empty (by importing) before creating the next, to get three distinct datasets.
	CSVImporter importer;
	const std::string csvPath = fq(_testLibrary().absoluteFilePath("csv/debug.csv"));

	DataSet * a = ws->createDataSet();
	QVERIFY(a);
	importer.loadDataSet(csvPath, a, [](int){});
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(csvPath, b, [](int){});
	DataSet * c = ws->createDataSet();
	QVERIFY(c);
	importer.loadDataSet(csvPath, c, [](int){});

	QVERIFY(a->id() != b->id());
	QVERIFY(b->id() != c->id());
	QVERIFY(a->id() != c->id());

	a->setCodeType(computedColumnType::rCode);
	b->setCodeType(computedColumnType::rCode);
	c->setCodeType(computedColumnType::rCode);

	std::string err;
	QVERIFY(!ws->computedDataSetsHaveLoop(err));

	//A valid chain c -> b -> a is accepted and is not a loop.
	QVERIFY(c->setDefaultInputFilterId(b->defaultFilter()->id()));
	QVERIFY(b->setDefaultInputFilterId(a->defaultFilter()->id()));
	QCOMPARE(c->defaultInputFilterId(), b->defaultFilter()->id());
	QCOMPARE(b->defaultInputFilterId(), a->defaultFilter()->id());
	QVERIFY(!ws->computedDataSetsHaveLoop(err));

	//A depending on C would close the chain into a loop (A <- C <- B <- A) and must be refused,
	//leaving A without an input (the value is unchanged).
	QVERIFY(!a->setDefaultInputFilterId(c->defaultFilter()->id()));
	QCOMPARE(a->defaultInputFilterId(), -1);

	//Likewise A depending on B while B depends on A is a loop and must be refused.
	QVERIFY(!a->setDefaultInputFilterId(b->defaultFilter()->id()));
	QCOMPARE(a->defaultInputFilterId(), -1);

	QVERIFY(!ws->computedDataSetsHaveLoop(err));
}

void TestAll::testUndoColumnDropLevels()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	Column * col = ds->column("contNormal");
	QVERIFY(col);

	UndoStack::setCurrent(ds->undoStack());

	col->setDropLevels(dropLevelsType::drop);
	QCOMPARE(col->dropLevels(), dropLevelsType::drop);

	//Regression: the old value used to be stored as an int (0/1/2) while undo/redo restore it via
	//dropLevelsTypeFromQString (which needs the enum name) -> undo threw missingEnumVal.
	ds->undoStack()->pushCommand(new SetColumnPropertyCommand(col,
		dropLevelsTypeToQString(dropLevelsType::keep),
		SetColumnPropertyCommand::ColumnProperty::DropLevels));

	QCOMPARE(col->dropLevels(), dropLevelsType::keep); //push() redoes the command

	ds->undoStack()->undo();
	QCOMPARE(col->dropLevels(), dropLevelsType::drop);

	ds->undoStack()->redo();
	QCOMPARE(col->dropLevels(), dropLevelsType::keep);
}

void TestAll::testEncoderPrefixPerDataset()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * a = _pkg->dataSet();
	QVERIFY(a);
	QVERIFY(a->columnCount() > 0);

	const std::string colName	= a->column(0)->name();
	const std::string prefixA	= "JASPColumn_" + std::to_string(a->id()) + "_";
	const std::string encodedA	= a->encoder().encode(colName);

	QVERIFY2(encodedA.find(prefixA) == 0,	qPrintable("Encoder prefix must carry the dataset id"));
	QVERIFY2(encodedA.find("-1") == std::string::npos,	qPrintable("Encoder prefix must not be the -1 sentinel"));

	//Reload from the DB (the .jasp restore path): the prefix must still carry the id, not -1.
	DataSet loadMe(nullptr, a->id());
	QCOMPARE(loadMe.id(), a->id());
	const std::string encodedReload = loadMe.encoder().encode(colName);
	QVERIFY2(encodedReload.find(prefixA) == 0,	qPrintable("Reloaded dataset must keep the id-based prefix"));
	QCOMPARE(encodedReload, encodedA);

	//A second dataset with a colliding column name must get a distinct prefix.
	CSVImporter importer;
	DataSet * b = _pkg->workspace()->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	QVERIFY(b->id() != a->id());

	const std::string prefixB	= "JASPColumn_" + std::to_string(b->id()) + "_";
	const std::string encodedB	= b->encoder().encode(b->column(0)->name());
	QVERIFY2(encodedB.find(prefixB) == 0,	qPrintable("Second dataset must get its own id-based prefix"));
	QVERIFY(encodedB != encodedA);

	//Instance-level JSON decode must work against the dataset's own encoder (the static
	//ColumnEncoder::decodeJson is a no-op on the desktop: the global current-encoder indirection is
	//only set inside the engine).
	Json::Value json;
	json["axis"] = encodedA;
	a->encoder().decodeJson(json);
	QCOMPARE(json["axis"].asString(), colName);
}

void TestAll::testFilterRemoveFilter()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	const size_t before = ds->filters().size();

	Filter * f = ds->createFilter("testRemoveMe", true);
	QVERIFY(f);
	QCOMPARE(ds->filters().size(), before + 1);
	QVERIFY(ds->filter("testRemoveMe") == f);

	ds->runFilters(); //must be safe while the filter is present

	//The default filter is not removable and must be a no-op.
	ds->removeFilter(ds->defaultFilter());
	QCOMPARE(ds->filters().size(), before + 1);

	ds->removeFilter(f);
	QCOMPARE(ds->filters().size(), before);
	QVERIFY(ds->filter("testRemoveMe") == nullptr);

	ds->runFilters(); //must still be safe after removal (the dangling-pointer regression would crash here)
}

void TestAll::testSyncerExportModifyReimportChangesDetected()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	// Create an initial CSV
	QString srcPath = tempDir.filePath("data.csv");
	QFile src(srcPath);
	QVERIFY(src.open(QIODevice::WriteOnly));
	src.write("x,y\n1,2\n3,4\n");
	src.close();

	// Import it
	CSVImporter importer;
	importer.loadDataSet(fq(srcPath), _pkg->createDataSet(), [](int){});
	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QCOMPARE(ds->rowCount(), 2);
	QCOMPARE(ds->columnCount(), 2);
	QCOMPARE(ds->column(0)->name(), "x");
	QCOMPARE(ds->column(1)->name(), "y");

	// Also export the original and verify
	QString exportPath = tempDir.filePath("export.csv");
	DataExporter exporter(false);
	exporter.saveDataSet(fq(exportPath), [](int){});
	QFile exp(exportPath);
	QVERIFY(exp.open(QIODevice::ReadOnly));
	QVERIFY(QString::fromUtf8(exp.readAll()).contains("x,y"));
	exp.close();

	// Now overwrite the source file with different content
	QFile modified(srcPath);
	QVERIFY(modified.open(QIODevice::WriteOnly));
	modified.write("x,z\n1,7\n3,8\n5,9\n");
	modified.close();

	// Reimport into a new dataset to verify fresh import picks up changes
	DataSet * ds2 = _pkg->createDataSet();
	QVERIFY(ds2);
	_pkg->workspace()->setShownDataSet(ds2);

	CSVImporter importer2;
	importer2.loadDataSet(fq(srcPath), ds2, [](int){});
	QCOMPARE(ds2->rowCount(), 3);
	QCOMPARE(ds2->columnCount(), 2);

	DataExporter exporter2(false);
	QString exportPath2 = tempDir.filePath("export2.csv");
	exporter2.saveDataSet(fq(exportPath2), [](int){});

	QFile exp2(exportPath2);
	QVERIFY(exp2.open(QIODevice::ReadOnly));
	QString content = QString::fromUtf8(exp2.readAll());
	exp2.close();

	QStringList lines = content.split('\n', Qt::SkipEmptyParts);
	QCOMPARE(lines.size(), 4); // header + 3 data rows
	QVERIFY(lines[0].contains("x"));
	QVERIFY(lines[0].contains("z"));
	QVERIFY(!lines[0].contains("y"));
	QVERIFY(lines[1].contains("7"));
	QVERIFY(lines[3].contains("9"));
}

void TestAll::testFilterRevisionInvalidatedRoundTrip()
{
	//Regression test: filterLoad used to assign `revision` twice (overwriting it with the
	//`invalidated` column) and never loaded `invalidated`. Save a filter and check every
	//field round-trips, especially revision vs invalidated.
	QVERIFY(_newPkgWithDataSet());
	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	DatabaseInterface & dbi = ds->db();
	const int dataSetId = ds->id();
	QVERIFY(dataSetId > 0);

	const std::string originalRFilter		= "filterResult <- x > 1";
	const std::string originalGenerated		= "generated <- TRUE";
	const std::string originalConstructor	= "{\"formulas\":[]}";
	const std::string originalConstructorR	= "constrR <- 1 + 1";
	const std::string originalName			= "roundtripFilter";

	const int filterId = dbi.filterInsert(dataSetId, originalRFilter, originalGenerated, originalConstructor, originalConstructorR, originalName);
	QVERIFY2(filterId > 0, "filterInsert should return a valid filter id");

	//Update with a marked-invalidated flag; make sure it round-trips.
	const std::string updatedRFilter		= "filterResult <- x > 2";
	const std::string updatedGenerated		= "generated <- FALSE";
	const std::string updatedConstructor	= "{\"formulas\":[1]}";
	const std::string updatedConstructorR	= "constrR <- 2 + 2";
	const std::string updatedName			= "roundtripFilterRenamed";
	const bool		updatedInvalidated		= true;

	dbi.filterUpdate(filterId, updatedRFilter, updatedGenerated, updatedConstructor, updatedConstructorR, updatedName, updatedInvalidated);

	std::string rFilter, generatedFilter, constructorJson, constructorR, name;
	int		revision		= -1;
	bool	invalidated		= false;

	dbi.filterLoad(filterId, rFilter, generatedFilter, constructorJson, constructorR, revision, name, invalidated);

	QCOMPARE(QString::fromStdString(rFilter),			QString::fromStdString(updatedRFilter));
	QCOMPARE(QString::fromStdString(generatedFilter),	QString::fromStdString(updatedGenerated));
	QCOMPARE(QString::fromStdString(constructorJson),	QString::fromStdString(updatedConstructor));
	QCOMPARE(QString::fromStdString(constructorR),		QString::fromStdString(updatedConstructorR));
	QCOMPARE(QString::fromStdString(name),				QString::fromStdString(updatedName));
	QCOMPARE(invalidated, updatedInvalidated);
	QVERIFY2(revision >= 0, "revision must stay an integer revision, not the invalidated flag");

	dbi.filterDelete(filterId);
}

void TestAll::testSyncKeepMissingColumns()
{
	_pkg = new DataSetPackage(this);

	//Importer::syncDataSet reads the preferences singleton; nothing else in the tests creates one.
	if(!PreferencesModel::prefs())
		new PreferencesModel(this);
	QVERIFY(PreferencesModel::prefs());

	//syncDataSet asks permission through checkDoSync; with no MainWindow around nothing answers it and
	//the signal would return a default-constructed false, aborting every sync below.
	connect(DataSetPackage::pkg(), &DataSetPackage::checkDoSync, this, &TestAll::_checkDoSyncFake, Qt::DirectConnection);

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
		DataSet * ds = _pkg->createDataSet();
		_pkg->workspace()->setShownDataSet(ds);

		CSVImporter importer;
		importer.loadDataSet(fq(csv), ds, [](int){});
		return ds;
	};

	//Without the preference a column that is gone from the new data file is removed.
	{
		PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(false);

		DataSet * ds = freshDataSetFrom(abc);
		QCOMPARE(ds->columnCount(), 3);

		CSVImporter syncer;
		syncer.syncDataSet(fq(ab), ds, [](int){});

		QCOMPARE(ds->columnCount(), 2);
		QVERIFY(!ds->column("c"));
	}

	//With the preference that same column is kept instead of removed.
	{
		PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(true);

		DataSet * ds = freshDataSetFrom(abc);
		QCOMPARE(ds->columnCount(), 3);

		CSVImporter syncer;
		syncer.syncDataSet(fq(ab), ds, [](int){});

		QCOMPARE(ds->columnCount(), 3);
		QVERIFY(ds->column("c"));

		//Syncing on against a file that shares no column at all keeps every one of them: p and q are added
		//next to a, b and c rather than taking their place, so the data holds the union of both files. That
		//union keeps growing for as long as one session goes on synchronizing, which is why each data file
		//normally gets a JASP process of its own.
		CSVImporter syncer2;
		syncer2.syncDataSet(fq(pq), ds, [](int){});

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

void TestAll::testFileSyncerFullAsyncFlow()
{
	//Test the complete FileEvent + AsyncLoader sync flow
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	//Create a test CSV file
	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("async_sync.csv");
	{
		QFile f(testFilePath);
		QVERIFY(f.open(QIODevice::WriteOnly));
		f.write("a,b,c\n1,2,3\n");
		f.close();
	}

	//Set the data file and timestamp so the sync has something to compare against
	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	//Create an AsyncLoader on heap so it lives during async operations
	AsyncLoader * loader = new AsyncLoader(this);
	QSignalSpy syncCompletedSpy(loader, &AsyncLoader::syncCompleted);

	//Connect DataSet::syncRequired to AsyncLoader::onSyncRequired (like MainWindow does)
	connect(ds, &DataSet::syncRequired, loader, &AsyncLoader::onSyncRequired, Qt::QueuedConnection);

	//Trigger sync
	DataSetSyncer & syncer = ds->syncer();
	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());

	//Update timestamp so fileChanged will pass the timestamp check when syncer.syncNow() is called
	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	//Trigger syncNow which will call fileChanged -> doSync -> emit syncRequired
	syncer.syncNow();

	//Wait for syncCompleted to be emitted by AsyncLoader (via syncRequired -> onSyncRequired -> loadPackage -> syncCompleted)
	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy.count(), 1, 3000);

	//Verify sync completed successfully
	QVERIFY(syncer.isFileSyncing()); //Still syncing because we haven't called stop
	QVERIFY(ds->dataFileSynch());

	syncer.stopFileSyncing();

	//Cleanup
	delete loader;
}

void TestAll::testSyncerDatabaseSyncFromSQLite()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	QString testDbPath = tempDir.filePath("test_sync.db");
	QVERIFY(!testDbPath.isEmpty());

	sqlite3 * db = nullptr;
	int ret = sqlite3_open(testDbPath.toStdString().c_str(), &db);
	QVERIFY2(ret == SQLITE_OK, QString("Failed to open/create database: %1").arg(testDbPath).toStdString().c_str());

	std::string createTableSql = "CREATE TABLE test_data ("
	                             "id INTEGER PRIMARY KEY, "
	                             "name TEXT, "
	                             "value REAL, "
	                             "category TEXT"
	                             ");";
	ret = sqlite3_exec(db, createTableSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to create table");

	std::string insertDataSql = "INSERT INTO test_data (id, name, value, category) VALUES "
	                            "(1, 'first', 10.5, 'A'), "
	                            "(2, 'second', 20.3, 'B'), "
	                            "(3, 'third', 30.7, 'A');";
	ret = sqlite3_exec(db, insertDataSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to insert initial data");

	sqlite3_close(db);
	db = nullptr;

	Json::Value dbJson;
	dbJson["dbType"] = "QSQLITE";
	dbJson["database"] = testDbPath.toStdString();
	dbJson["query"] = "SELECT id, name, value, category FROM test_data";
	dbJson["interval"] = 1;

	syncer.startDatabaseSyncing(dbJson, false);
	QVERIFY(syncer.isDatabaseSyncing());
	QVERIFY(ds->isDatabase());

	AsyncLoader * loader = new AsyncLoader(this);
	QSignalSpy syncCompletedSpy(loader, &AsyncLoader::syncCompleted);
	connect(ds, &DataSet::syncRequired, loader, &AsyncLoader::onSyncRequired, Qt::QueuedConnection);

	// Fake the checkDoSync signal to return true (no MainWindow in tests)
	connect(DataSetPackage::pkg(), &DataSetPackage::checkDoSync, this, &TestAll::_checkDoSyncFake, Qt::DirectConnection);

	QSignalSpy syncRequiredSpy(&syncer, &DataSetSyncer::syncRequired);
	QSignalSpy startedSpy(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy(&syncer, &DataSetSyncer::syncingFinished);

	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy.count(), 1, 3000);
	syncer.setSyncingResult(true);
	QTRY_COMPARE_WITH_TIMEOUT(finishedSpy.count(), 1, 3000);

	QTRY_COMPARE_WITH_TIMEOUT(syncRequiredSpy.count(), 1, 3000);

	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy.count(), 1, 3000);

	QVERIFY(ds->columnCount() >= 4);
	QVERIFY(ds->rowCount() == 3);

	Column * idCol = ds->column("id");
	Column * nameCol = ds->column("name");
	Column * valueCol = ds->column("value");
	Column * categoryCol = ds->column("category");

	QVERIFY(idCol);
	QVERIFY(nameCol);
	QVERIFY(valueCol);
	QVERIFY(categoryCol);

	// Basic verification that data was loaded
	QVERIFY((*idCol)[0] == "1");
	QVERIFY((*valueCol)[0] == "10.5");

	db = nullptr;
	ret = sqlite3_open(testDbPath.toStdString().c_str(), &db);
	QVERIFY2(ret == SQLITE_OK, "Failed to reopen database for modification");

	std::string updateDataSql = "UPDATE test_data SET value = value + 5 WHERE id IN (1, 2, 3);";
	ret = sqlite3_exec(db, updateDataSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to update values");

	sqlite3_close(db);
	db = nullptr;

	QSignalSpy syncCompletedSpy2(loader, &AsyncLoader::syncCompleted);
	QSignalSpy syncRequiredSpy2(&syncer, &DataSetSyncer::syncRequired);
	QSignalSpy startedSpy2(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy2(&syncer, &DataSetSyncer::syncingFinished);

	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy2.count(), 1, 3000);
	syncer.setSyncingResult(true);
	QTRY_COMPARE_WITH_TIMEOUT(finishedSpy2.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncRequiredSpy2.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy2.count(), 1, 3000);

	QVERIFY((*valueCol)[0] == "15.5");
	QVERIFY((*valueCol)[1] == "25.3");
	QVERIFY((*valueCol)[2] == "35.7");

	// Note: Database sync doesn't currently support dynamic schema changes
	// Testing value updates only - schema changes would require re-sync from database

	// Final value update test
	db = nullptr;
	ret = sqlite3_open(testDbPath.toStdString().c_str(), &db);
	QVERIFY2(ret == SQLITE_OK, "Failed to reopen database for final value update");

	std::string finalUpdateSql = "UPDATE test_data SET value = 99.9 WHERE id = 2;";
	ret = sqlite3_exec(db, finalUpdateSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to final update");

	sqlite3_close(db);
	db = nullptr;

	QSignalSpy syncCompletedSpy4(loader, &AsyncLoader::syncCompleted);
	QSignalSpy syncRequiredSpy4(&syncer, &DataSetSyncer::syncRequired);
	QSignalSpy startedSpy4(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy4(&syncer, &DataSetSyncer::syncingFinished);

	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy4.count(), 1, 3000);
	syncer.setSyncingResult(true);
	QTRY_COMPARE_WITH_TIMEOUT(finishedSpy4.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncRequiredSpy4.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy4.count(), 1, 3000);

	QVERIFY((*valueCol)[1] == "99.9");

	syncer.stopDatabaseSyncing();

	QVERIFY(!syncer.isDatabaseSyncing());
	QVERIFY(!ds->isDatabase());

	delete loader;
}

void TestAll::testCloseWorkspaceAndDataSets()
{
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	QVERIFY(_pkg->dataSet());

	//Give the workspace several distinct (non-empty) datasets so deleteShownDataSet has to
	//re-pick another shown dataset after each removal.
	CSVImporter importer;
	const std::string csvPath = fq(_testLibrary().absoluteFilePath("csv/debug.csv"));

	DataSet * second = ws->createDataSet();
	QVERIFY(second);
	importer.loadDataSet(csvPath, second, [](int){});
	QVERIFY(second->columnCount() > 0);

	DataSet * third = ws->createDataSet();
	QVERIFY(third);
	importer.loadDataSet(csvPath, third, [](int){});
	QVERIFY(third->columnCount() > 0);

	QCOMPARE(ws->dataSets().size(), size_t(3));

	//Deleting the shown dataset must not crash and must leave the other datasets alive.
	DataSet * shown = ws->shownDataSet();
	QVERIFY(shown);
	ws->deleteShownDataSet();
	QCOMPARE(ws->dataSets().size(), size_t(2));
	QVERIFY(ws->shownDataSet());
	QVERIFY(ws->shownDataSet() != shown);

	//Delete the remaining ones, one at a time, until the workspace is empty. The old crash
	//(ColumnModel::shownDataSetChangedHandler disconnecting a stale dataset) used to segfault here.
	while (ws->shownDataSet())
		ws->deleteShownDataSet();

	QCOMPARE(ws->dataSets().size(), size_t(0));
	QVERIFY(!ws->shownDataSet());

	//Re-populate, then tear the whole workspace down (deleteWorkspace/reset) — must not crash either.
	DataSet * again = ws->createDataSet();
	QVERIFY(again);
	importer.loadDataSet(csvPath, again, [](int){});
	QVERIFY(ws->dataSets().size() == size_t(1));

	_pkg->deleteWorkspace();
	QVERIFY(!_pkg->workspace());

	//A fresh workspace (as DataSetPackage::createDataSet does on first use) still works afterwards.
	DataSet * fresh = _pkg->createDataSet();
	QVERIFY(fresh);
	QVERIFY(_pkg->workspace());

	//_pkg->deleteWorkspace() above destroyed the workspace `ws` pointed at; createDataSet() made a new
	//one, so re-obtain it before touching it.
	ws = _pkg->workspace();
	QVERIFY(ws);

	//Regression: after closing the workspace, opening (i.e. adding) datasets again must keep working
	//instead of targeting a stale/removed workspace. Load data into the fresh dataset and add a couple
	//more, then make sure the workspace holds them all and can still close them without crashing.
	importer.loadDataSet(csvPath, fresh, [](int){});
	QVERIFY(fresh->columnCount() > 0);

	DataSet * secondAfterClose = ws->createDataSet();
	QVERIFY(secondAfterClose);
	importer.loadDataSet(csvPath, secondAfterClose, [](int){});
	QVERIFY(secondAfterClose->columnCount() > 0);

	DataSet * thirdAfterClose = ws->createDataSet();
	QVERIFY(thirdAfterClose);
	importer.loadDataSet(csvPath, thirdAfterClose, [](int){});
	QVERIFY(thirdAfterClose->columnCount() > 0);

	QCOMPARE(ws->dataSets().size(), size_t(3));
	QVERIFY(ws->shownDataSet());

	while (ws->shownDataSet())
		ws->deleteShownDataSet();

	QCOMPARE(ws->dataSets().size(), size_t(0));
	QVERIFY(!ws->shownDataSet());
}

bool TestAll::_checkDoSyncFake()
{
	return true;
}

bool TestAll::_writeTextFile(const QString & path, const QByteArray & contents)
{
	QFile file(path);
	if(!file.open(QIODevice::WriteOnly | QIODevice::Text))
		return false;
	return file.write(contents) == contents.size();
}

//No modules are loaded in this backendless test process, so every module-backed analysis in a jasp
//file reports that its module is missing and the batch run exits non-zero for that alone. Anything
//else in the report means the chain under test actually went wrong.
bool TestAll::_batchErrorsAreOnlyMissingModules(MainWindow * mw)
{
	for(const QString & error : mw->_batchResult.errors)
		if(!error.contains("Module is not available"))
		{
			qWarning() << "Unexpected batch error:" << error;
			return false;
		}

	return true;
}

//Constructs a full MainWindow the way the command line sees it and detaches exitSignal from
//QApplication::exit (which the constructor wires up and which would quit the test event loop),
//returning a spy on the signal instead. The spy is parented to nothing; delete it after use.
QSignalSpy * TestAll::_newMainWindowWithExitSpy(MainWindow *& mw)
{
	//A previous MainWindow teardown took the whole session directory (and its database) with it,
	//while MainWindow wants to create its own DatabaseInterface in there again.
	TempFiles::init(ProcessInfo::currentPID());

	//Keep the QML engine and the R engines out of the test process: the data/sync chain under test
	//does not need them (no backend at all, though the tests do run under a display), and both add
	//many threads (sqlite contention, JS garbage collection) that make the test flaky. See
	//backendlessTestMode() in mainwindow.cpp.
	qputenv("JASP_TEST_BACKENDLESS", "1");

	//Other tests may have left the global PreferencesModel singleton behind; the MainWindow wants
	//to construct its own, and a second one asserts.
	delete PreferencesModel::prefs();

	QCoreApplication::setApplicationName("JASPTest"); //so checkForUpdates() stays out of the way

	try
	{
		//In batch mode, because that is what the command-line chain under test is: since #6318 the
		//chain ends in finishBatchRun(), which returns right away when the window was not started
		//for a batch, so without this nothing ever exits.
		mw = new MainWindow(nullptr, true);
	}
	catch(const std::exception & e)
	{
		std::cerr << "MainWindow construction threw: " << e.what() << std::endl;
		throw;
	}

	//The MainWindow listens for the interactive CSV preview; with no delimiter preset the importers
	//would then block this (GUI) thread on an answer that can never come (regression guard: the
	//per-test init() resets it to '\0').
	DesktopCommunicator::singleton()->setKnownCsvDelimiter(',');

	disconnect(mw, &MainWindow::exitSignal, mw, nullptr); //keep every other listener, lose only qApp

	return new QSignalSpy(mw, &MainWindow::exitSignal);
}

void TestAll::testCliSyncExportChainFromFreshWorkspace()
{
	//A genuine JASP file from the test library, so the chain runs against a real saved document:
	const QString	jaspPath	= _testLibrary().absoluteFilePath("jasp/Descriptives-Debug.jasp");

	QVERIFY(QFileInfo::exists(jaspPath));

	//The sync data holds exactly the columns of that file (in its order), so synchronizing
	//replaces them without adding or deleting any: this pins the plain open -> sync -> exit chain.
	const QByteArray syncCsvData =
		"V1,contNormal,contGamma,contBinom,contExpon,contWide,contNarrow,contOutlier,contcor1,contcor2,"
		"facGender,facExperim,facFive,facFifty,facOutlier,debString,debMiss1,debMiss30,debMiss80,debMiss99,"
		"debBinMiss20,debNaN,debNaN10,debInf,debCollin1,debCollin2,debCollin3,debEqual1,debEqual2,debSame,unicode\n"
		"1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,sixteen,17,18,19,20,21,22,23,24,25,26,27,28,29,thirty\n"
		"31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,fortysix,47,48,49,50,51,52,53,54,55,56,57,58,59,sixty\n";

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	//A scratch folder inside JASP's own session directory: QTemporaryDir can trigger a permission
	//dialog on some systems, while TempFiles already has a session dir set up by
	//_newMainWindowWithExitSpy. The MainWindow teardown removes the session dir, so no manual
	//cleanup of the folder or its file is needed.
	const QString	syncCsv		= QString::fromStdString(TempFiles::createTmpFolder()) + "/syncdata.csv";

	QVERIFY(_writeTextFile(syncCsv, syncCsvData));

	//The results page finished loading before the command line triggers the file open, and nothing
	//has been loaded into the workspace yet. Note that _open's dataset check still passes, because
	//JASP startup itself creates an empty dataset; the chain therefore binds the *loaded* dataset
	//only after the open has finalized, which is exactly what chain(..., resetDataSet=true) does.
	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, syncCsv, "", false);

	//The open -> synchronize chain should finish by exiting JASP, and complain about nothing but the
	//modules this process does not have:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QVERIFY(_batchErrorsAreOnlyMissingModules(mw));
	QCOMPARE(exitSpy->first().first().toInt(), mw->_batchResult.errors.isEmpty() ? 0 : 1);

	//And the workspace should hold the synchronized data:
	DataSet * synced = DataSetPackage::pkg()->dataSet();
	QVERIFY(synced);
	QVERIFY(synced->column("contNormal"));

	try
	{
		delete exitSpy;
		delete mw; //MainWindow::singleton() and other singletons must not survive into the next test
	}
	catch(const std::exception & e)
	{
		std::cerr << "MainWindow teardown threw: " << e.what() << std::endl;
		throw;
	}

	//The MainWindow teardown took the session directory (and its database) with it; following tests
	//still expect it to exist.
	TempFiles::init(ProcessInfo::currentPID());
}

void TestAll::testCliSyncExportWaitsForAnalysesToSettle()
{
	//Regression for the export capturing the intermediate emptied results: synchronizing can
	//schedule analysis refreshes deferred, and a refresh that starts *after* the export began
	//wipes the results page to its empty in-between state while the export is capturing it.
	//The export must therefore wait until the analyses have stopped changing status. Without a
	//webview the exported HTML itself cannot be checked here, so this test simulates the wipe by
	//flipping an analysis' status right after the export was queued and verifies the export only
	//starts once the analysis is finished again.
	const QString	jaspPath	= _testLibrary().absoluteFilePath("jasp/Descriptives-Debug.jasp");

	QVERIFY(QFileInfo::exists(jaspPath));

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	//Short-circuit the exporter's wait for the (nonexistent) webview: prepForExport is emitted from
	//the exporter thread and only the QML results page would answer it, so answer it here instead.
	connect(mw->_resultsJsInterface, &ResultsJsInterface::prepForExport, mw->_resultsJsInterface, &ResultsJsInterface::exportPrepFinished, Qt::QueuedConnection);

	const QString	syncCsv		= QString::fromStdString(TempFiles::createTmpFolder()) + "/syncdata.csv",
					outHtml		= QString::fromStdString(TempFiles::createTmpFolder()) + "/results.html";

	QVERIFY(_writeTextFile(syncCsv, "V1\n1\n2\n"));

	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, syncCsv, outHtml, false);

	//The open -> synchronize chain queues an export that waits for the analyses:
	QTRY_VERIFY_WITH_TIMEOUT(mw->_waitingEvent != nullptr, 30000);
	QVERIFY(!mw->_waitingEvent->isStarted()); //the settle-debounce guarantees it cannot start this early

	//In backendless mode the module-backed analyses from the jasp file are skipped, so inject a
	//report-style analysis (created without a module, and never run: Analysis::run ignores reports):
	Json::Value analysisData;
	analysisData["id"]		= 1;
	analysisData["title"]	= "SettleTest";
	analysisData["isReport"]= true;
	analysisData["status"]	= "complete";
	Analysis * analysis = mw->_analyses->createFromJaspFileEntry(analysisData, nullptr);
	QVERIFY(analysis);
	QVERIFY(mw->_analyses->allFinished());

	//Simulate a refresh starting (status Complete -> Empty, like Analysis::run() does) right after
	//the export was queued: the pending start must be postponed until the analyses finish again:
	analysis->setStatus(Analysis::Empty);
	QVERIFY(mw->_waitingEvent); //not consumed by a premature start
	QVERIFY(!mw->_waitingEvent->isStarted());

	analysis->setStatus(Analysis::Complete);

	//Only now may the export start, which is visible as the waiting event being taken over:
	QTRY_VERIFY_WITH_TIMEOUT(mw->_waitingEvent == nullptr, 10000);

	//The exporter waits for the (nonexistent) webview to deliver the HTML, and ResultsJsInterface::
	//exportHTML resets the ready flag right before that wait; keep setting it so the exporter sees
	//it ready no matter when exactly its wait starts (it dies with mw):
	QTimer * readySetter = new QTimer(mw);
	readySetter->setInterval(100);
	connect(readySetter, &QTimer::timeout, this, [](){ DataSetPackage::pkg()->setAnalysesHTMLReady(); });
	readySetter->start();

	//The export finishing is what exits JASP, again complaining about nothing but the missing modules:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QVERIFY(_batchErrorsAreOnlyMissingModules(mw));
	QCOMPARE(exitSpy->first().first().toInt(), mw->_batchResult.errors.isEmpty() ? 0 : 1);
	QVERIFY(QFileInfo::exists(outHtml));

	delete exitSpy;
	delete mw; //MainWindow::singleton() and other singletons must not survive into the next test

	//The MainWindow teardown took the session directory (and its database) with it; following tests
	//still expect it to exist.
	TempFiles::init(ProcessInfo::currentPID());
}

void TestAll::testCliSyncExportChainFailsOnBadDataFile()
{
	const QString	jaspPath	= _testLibrary().absoluteFilePath("jasp/Descriptives-Debug.jasp");

	QVERIFY(QFileInfo::exists(jaspPath));

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	//A scratch folder inside JASP's own session directory: QTemporaryDir can trigger a permission
	//dialog on some systems, while TempFiles already has a session dir set up by
	//_newMainWindowWithExitSpy. The MainWindow teardown removes the session dir, so no manual
	//cleanup of the folder or its file is needed.
	const QString	bogusData	= QString::fromStdString(TempFiles::createTmpFolder()) + "/bogus.xlsx";

	QVERIFY(_writeTextFile(bogusData, "this is not a spreadsheet\n"));

	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, bogusData, "", false);

	//A failed synchronization must exit JASP with a non-zero code instead of continuing as if the
	//synchronization had succeeded:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QVERIFY(exitSpy->first().first().toInt() != 0);

	//...and for the right reason: the modules missing from this backendless process already make the
	//run non-zero, so check the import failure is actually in the report.
	QVERIFY2(!mw->_batchResult.errors.filter("Could not import data").isEmpty(),
			 qPrintable("Batch errors: " + mw->_batchResult.errors.join(" | ")));

	delete exitSpy;
	delete mw; //MainWindow::singleton() and other singletons must not survive into the next test

	//The MainWindow teardown took the session directory (and its database) with it; following tests
	//still expect it to exist.
	TempFiles::init(ProcessInfo::currentPID());
}


QTEST_MAIN(TestAll)
