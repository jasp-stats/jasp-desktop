//
// Copyright (C) 2026 University of Amsterdam
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
#include "testcsvpreviewmodel.h"
#include "numbersinlocales.h"
#include "utilities/csvpreviewmodel.h"
#include "utilities/desktopcommunicator.h"
#include "utilities/languagemodel.h"
#include "utilities/settings.h"
#include "utilities/qutils.h"
#include "columnutils.h"
#include <QLocale>
#include <QScopeGuard>

void TestCsvPreviewModel::initTestCase()
{
	//CsvPreviewModel offers the languages and territories that LanguageModel knows about, so there has to be one
	Settings::informSettingsThatThisIsATest();
	_languageModel = new LanguageModel(nullptr, nullptr, this);
}


void TestCsvPreviewModel::testCsvParsing()
{
    CsvPreviewModel model;
    
	QString rawData = "Col1,Col2,Col3\nVal1,Val2,Val3\n1.2,1.20,1.344";
    model.preparePreview(rawData.toStdString().c_str(), ',');

    QCOMPARE(model.rowCount(), 3);
    QCOMPARE(model.columnCount(), 3);

    // Check first row (header)
    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1"));
    QCOMPARE(model.data(model.index(0, 1), Qt::DisplayRole).toString(), QString("Col2"));
    QCOMPARE(model.data(model.index(0, 2), Qt::DisplayRole).toString(), QString("Col3"));

    // Check second row
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("\"Val1\""));
	QCOMPARE(model.data(model.index(1, 1), Qt::DisplayRole).toString(), QString("\"Val2\""));
	QCOMPARE(model.data(model.index(1, 2), Qt::DisplayRole).toString(), QString("\"Val3\""));

    // Check third row
	QCOMPARE(model.data(model.index(2, 0), Qt::DisplayRole).toString(), QString("1.2"));
	QCOMPARE(model.data(model.index(2, 1), Qt::DisplayRole).toString(), QString("1.2"));
	QCOMPARE(model.data(model.index(2, 2), Qt::DisplayRole).toString(), QString("1.344"));

    // Test delimiter change to wrong delimiter
    model.preparePreview(rawData.toStdString().c_str(), ';');
    QCOMPARE(model.rowCount(), 3);
    QCOMPARE(model.columnCount(), 1);
    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1,Col2,Col3"));

    // Test delimiter change back to correct one
    model.preparePreview(rawData.toStdString().c_str(), ',');
    QCOMPARE(model.columnCount(), 3);
    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1"));
}

void TestCsvPreviewModel::testDifferentDelimiters()
{
    CsvPreviewModel model;

    // Semicolon delimiter
	QString semicolonData = "Col1;Col2;Col3\n1,2;1,234;1.2";
    model.preparePreview(semicolonData.toStdString().c_str(), ';');
    QCOMPARE(model.columnCount(), 3);
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("\"1,2\""));
	//A comma with three digits behind it groups thousands in English, the language of the interface these tests run with, so this is a number and not a label
	QCOMPARE(model.data(model.index(1, 1), Qt::DisplayRole).toString(), QString("1234"));
	QCOMPARE(model.data(model.index(1, 2), Qt::DisplayRole).toString(), QString("1.2"));

    // Tab delimiter
    QString tabData = "Col1\tCol2\tCol3\nVal1\tVal2\tVal3";
    model.preparePreview(tabData.toStdString().c_str(), '\t');
    QCOMPARE(model.columnCount(), 3);
    QCOMPARE(model.data(model.index(0, 2), Qt::DisplayRole).toString(), QString("Col3"));
}

///The preview reads numbers with the locale chosen in the dialog, not with the one of the interface.
///A dot means something very different in German than it does in English.
void TestCsvPreviewModel::testImportLocale()
{
	CsvPreviewModel model;

	LanguageModel * languages = LanguageModel::lang();

	const QString	german	= languages->entryNameForLocale(QLocale(QLocale::German)),
					english	= languages->entryNameForLocale(QLocale(QLocale::English));

	//The short list is the very same one the preferences offer, so entries carry the language code: "de - Deutsch"
	QVERIFY(!german.isEmpty());
	QVERIFY( german.startsWith("de - "));
	QCOMPARE(model.languages(), languages->languageEntryNames());
	QVERIFY( model.languages().contains(german));
	QVERIFY( model.languages().contains(english));

	model.preparePreview("Col1,Col2\n86.298,1.2", ',');

	//The dropdown opens on language(), so languages() has to offer it
	QVERIFY2(model.languages().contains(model.language()), qPrintable("languages() does not offer " + model.language()));

	//In English a dot is a decimal point, so this column holds eighty-six point something
	model.setLanguage(english);
	QCOMPARE(model.language(),		english);
	QCOMPARE(model.importLocale().language(), QLocale::English);
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("86.298"));

	//In German that same dot is a thousands separator, so this column is eighty-six thousand, without reloading the file
	model.setLanguage(german);
	QCOMPARE(model.language(),		german);
	QCOMPARE(model.importLocale().language(), QLocale::German);
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("86298"));

	//The example tells the user exactly that, before they press Load
	QVERIFY(model.parseExample().contains("86298"));

	model.setLanguage(english);
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("86.298"));
}

///Closing the dialog has to hand the locale to the importer, which is waiting on another thread
void TestCsvPreviewModel::testImportLocaleIsHandedToTheImporter()
{
	CsvPreviewModel model;

	DesktopCommunicator::singleton()->setKnownImportLocale(std::nullopt);
	QVERIFY(!DesktopCommunicator::singleton()->knownImportLocale());

	model.preparePreview("Col1,Col2\n86.298,1.2", ',');
	model.setLanguage(LanguageModel::lang()->entryNameForLocale(QLocale(QLocale::German)));

	model.setVisible(false); //What pressing Load or Cancel comes down to

	QVERIFY(DesktopCommunicator::singleton()->knownImportLocale());
	QCOMPARE(DesktopCommunicator::singleton()->knownImportLocale()->language(), QLocale::German);

	DesktopCommunicator::singleton()->setKnownImportLocale(std::nullopt);
}

///The dialog opens on whatever language the preferences are set to, also when they were changed since the previous import
void TestCsvPreviewModel::testLocaleFallsBackOnTheInterface()
{
	CsvPreviewModel model;

	LanguageModel * languages = LanguageModel::lang();

	const QString german = languages->entryNameForLocale(QLocale(QLocale::German));

	model.preparePreview("Col1,Col2\n86.298,1.2", ',');

	//Straight out of the dialog, before anyone touched anything
	QCOMPARE(model.language(),		languages->currentLanguage());
	QCOMPARE(model.importLocale(),	languages->localeForEntryName(languages->currentLanguage()));

	model.setLanguage(german);
	QCOMPARE(model.importLocale().language(), QLocale::German);

	//Which is where every import starts, see preparePreview below
	model.resetLocaleToInterface();

	QCOMPARE(model.language(),		languages->currentLanguage());
	QCOMPARE(model.importLocale(),	languages->localeForEntryName(languages->currentLanguage()));

	//And opening the dialog again re-reads the preferences rather than keeping what the previous import used,
	//which is what makes a language changed in Preferences/Interface show up here as the new default
	model.setLanguage(german);
	QCOMPARE(model.importLocale().language(), QLocale::German);

	model.preparePreview("Col1,Col2\n86.298,1.2", ',');

	QCOMPARE(model.language(),		languages->currentLanguage());
	QCOMPARE(model.importLocale(),	languages->localeForEntryName(languages->currentLanguage()));
}

///Hundreds of languages are hard to pick from, so only the ones JASP itself speaks are offered until More languages is ticked
void TestCsvPreviewModel::testMoreLanguagesWidensTheLanguageList()
{
	CsvPreviewModel model;

	LanguageModel * languages = LanguageModel::lang();

	model.preparePreview("Col1,Col2\n86.298,1.2", ',');

	QVERIFY(!model.moreLanguages());

	const QStringList shortList = model.languages();

	QCOMPARE(shortList, languages->languageEntryNames());

	model.setMoreLanguages(true);

	const QStringList wholeRange = model.languages();

	QCOMPARE(wholeRange, languages->altLanguages());
	QVERIFY (wholeRange.size() > shortList.size());

	//The whole range names its languages natively, so the same locale is shown under another name and has to stay selectable
	QVERIFY2(wholeRange.contains(model.language()), qPrintable("the whole range does not offer " + model.language()));

	//A language JASP is not translated into can only be picked while More languages is on, so folding it away has to let go of it again
	QString notTranslatedInto;

	for(const QString & language : wholeRange)
		if(languages->entryNameForLocale(languages->localeForNames(language, "")).isEmpty())
		{
			notTranslatedInto = language;
			break;
		}

	QVERIFY(!notTranslatedInto.isEmpty());

	model.setLanguage(notTranslatedInto);
	QCOMPARE(model.language(), notTranslatedInto);

	model.setMoreLanguages(false);

	QVERIFY (model.languages().contains(model.language()));
	QCOMPARE(model.language(),		languages->currentLanguage());
	QCOMPARE(model.importLocale(),	languages->localeForEntryName(languages->currentLanguage()));

	//A language JASP does speak survives the switch both ways, only its name changes with the list
	model.setMoreLanguages(true);
	model.setLanguage(QLocale(QLocale::German).nativeLanguageName());
	QCOMPARE(model.importLocale().language(), QLocale::German);

	model.setMoreLanguages(false);
	QCOMPARE(model.language(),					languages->entryNameForLocale(QLocale(QLocale::German)));
	QCOMPARE(model.importLocale().language(),	QLocale::German);
}

///Text the chosen locale does not take for a number stays text, also when the locale of the interface would read it,
///and the preview shows exactly that: it reads numbers the way the import will (CSVImportColumn::valueLookup)
void TestCsvPreviewModel::testChosenLocaleIsNotOverruledByTheInterface()
{
	ColumnUtils::setExtraStringToNumber(QColumnUtils::stringToDoubleFor(QLocale(QLocale::English, QLocale::UnitedStates)), nullptr); //An interface reading "1,234.56" just fine
	auto backToTheInterface = qScopeGuard([]{ LanguageModel::lang()->setDefaultLocaleFromCurrent(); });

	CsvPreviewModel model;

	model.preparePreview("Col1\n1,234.56\n1.234,56", ';');
	model.setLanguage(LanguageModel::lang()->entryNameForLocale(QLocale(QLocale::German)));

	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("\"1,234.56\""));
	QCOMPARE(model.data(model.index(2, 0), Qt::DisplayRole).toString(), QString("1234.56"));
	QVERIFY (model.parseExample().contains("1,234.56  \u2192  text"));
}

///Some names in the complete list stand for a single territory: "español de México" is es_MX, where a comma groups thousands,
///not es_ES where it is the decimal point. Picking another territory afterwards gives that territory, under whatever name it goes by there.
void TestCsvPreviewModel::testRegionalLanguageNames()
{
	CsvPreviewModel model;

	model.preparePreview("Col1\n1,234", ';');
	model.setMoreLanguages(true);

	const QLocale mexico(QLocale::Spanish, QLocale::Mexico),
				  spain( QLocale::Spanish, QLocale::Spain);

	model.setLanguage(mexico.nativeLanguageName());

	QCOMPARE(model.importLocale(),	mexico);
	QCOMPARE(model.language(),		mexico.nativeLanguageName());
	QCOMPARE(model.territory(),		mexico.nativeTerritoryName());
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("1234"));

	model.setTerritory(spain.nativeTerritoryName());

	QCOMPARE(model.importLocale(),	spain);
	QCOMPARE(model.language(),		spain.nativeLanguageName());
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("1.234"));
}

///Every combination of an interface in English, German or French, with and without thousand separators, and a file picked to be written in English, German or French
void TestCsvPreviewModel::testNumbersInEveryLanguage_data()
{
	QTest::addColumn<QLocale>(	"interface");
	QTest::addColumn<bool>(		"thousandSeparators");
	QTest::addColumn<QLocale>(	"file");

	for(const QLocale & interface : NumbersInLocales::locales())
		for(bool thousandSeparators : { false, true })
			for(const QLocale & file : NumbersInLocales::locales())
				QTest::addRow("%s interface%s, %s file", qPrintable(QLocale::languageToString(interface.language())), thousandSeparators ? " with thousand separators" : "", qPrintable(QLocale::languageToString(file.language())))
					<< interface << thousandSeparators << file;
}

///The preview reads every number in the language picked for the file, exactly like the import will (see TestAll::testCsvImportNumbers),
///and shows it the way the interface does, whichever combination of the two it is. What is no number is shown between quotes, as text.
void TestCsvPreviewModel::testNumbersInEveryLanguage()
{
	QFETCH(QLocale,	interface);
	QFETCH(bool,	thousandSeparators);
	QFETCH(QLocale,	file);

	QColumnUtils::setCallbacksAndDefaultLocale(interface, thousandSeparators);
	auto backToTheInterface = qScopeGuard([]{ LanguageModel::lang()->setDefaultLocaleFromCurrent(); });

	const std::vector<NumbersInLocales::Sample> & samples = NumbersInLocales::samples();

	QString csv = "sample";
	for(const NumbersInLocales::Sample & sample : samples)
		csv += "\n" + QString::fromUtf8(sample.written);

	CsvPreviewModel model;

	model.preparePreview(csv, ';');

	const QString language = LanguageModel::lang()->entryNameForLocale(file);
	QVERIFY2(!language.isEmpty(), qPrintable("JASP speaks no " + QLocale::languageToString(file.language())));

	model.setLanguage(language);
	QCOMPARE(model.importLocale().language(), file.language());

	QStringList misread;

	for(size_t i=0; i<samples.size(); i++)
	{
		const QString	written		= QString::fromUtf8(samples[i].written);
		const double	number		= samples[i].readIn(file);
		const QString	shouldShow	= std::isnan(number) ? "\"" + written + "\"" : NumbersInLocales::shownIn(interface, number, thousandSeparators),
						shows		= model.data(model.index(int(i) + 1, 0), Qt::DisplayRole).toString();

		if(shows != shouldShow)
			misread.push_back(QString("\"%1\" should show as %2, but shows as %3").arg(written, shouldShow, shows));
	}

	QVERIFY2(misread.isEmpty(), qPrintable("\n" + misread.join("\n")));
}

QTEST_MAIN(TestCsvPreviewModel)
