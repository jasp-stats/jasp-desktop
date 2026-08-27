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
#include "testparsedarguments.h"
#include "parsedarguments.h"
#include "utilenums.h"

#include <QTemporaryDir>
#include <QFile>
#include <QFileInfo>

namespace {

// Builds an argc/argv array from a list of argument strings.
// argv[0] is always the program name "JASP".
struct ArgArray
{
	std::vector<QByteArray>	_storage;
	std::vector<char*>		_argv;
	int						argc;

	ArgArray(const QStringList& args)
	{
		_storage.reserve(args.size() + 1);
		_storage.push_back(QByteArray("JASP"));
		for (const QString& s : args)
			_storage.push_back(s.toUtf8());

		_argv.reserve(_storage.size());
		for (QByteArray& ba : _storage)
			_argv.push_back(ba.data());

		argc = static_cast<int>(_argv.size());
	}

	char** argv() { return _argv.data(); }
};

QString createTempJaspFile(const QTemporaryDir& dir, const QString& name = "test.jasp")
{
	QString path = dir.path() + "/" + name;
	QFile f(path);
	f.open(QIODevice::WriteOnly);
	f.close();
	return path;
}

QString createTempDataFile(const QTemporaryDir& dir, const QString& name = "data.csv")
{
	QString path = dir.path() + "/" + name;
	QFile f(path);
	f.open(QIODevice::WriteOnly);
	f.write("a,b\n1,2\n");
	f.close();
	return path;
}

} // namespace


// ── isDataFileType(FileType) ───────────────────────────────────────────────

void TestParsedArguments::testIsDataFileType()
{
	// Types that ARE data files
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::csv));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::txt));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::tsv));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::sav));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::ods));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::xlsx));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::xls));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::rdata));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::rds));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::dta));
	QVERIFY(ParsedArguments::isDataFileType(Utils::FileType::zsav));

	// Types that are NOT data files
	QVERIFY(!ParsedArguments::isDataFileType(Utils::FileType::empty));
	QVERIFY(!ParsedArguments::isDataFileType(Utils::FileType::unknown));
	QVERIFY(!ParsedArguments::isDataFileType(Utils::FileType::jasp));
	QVERIFY(!ParsedArguments::isDataFileType(Utils::FileType::html));
	QVERIFY(!ParsedArguments::isDataFileType(Utils::FileType::pdf));
	QVERIFY(!ParsedArguments::isDataFileType(Utils::FileType::database));
}

// ── isDataFileType(QString) ────────────────────────────────────────────────

void TestParsedArguments::testIsDataFileTypeFromPath()
{
	QVERIFY( ParsedArguments::isDataFileType("data.csv"));
	QVERIFY( ParsedArguments::isDataFileType("data.sav"));
	QVERIFY( ParsedArguments::isDataFileType("data.xlsx"));
	QVERIFY( ParsedArguments::isDataFileType("archive.rdata"));
	QVERIFY(!ParsedArguments::isDataFileType("results.jasp"));
	QVERIFY(!ParsedArguments::isDataFileType("output.html"));
	QVERIFY(!ParsedArguments::isDataFileType("output.pdf"));
	QVERIFY(!ParsedArguments::isDataFileType("noextension"));
}

// ── constructor — no arguments ─────────────────────────────────────────────

void TestParsedArguments::testNoArguments()
{
	ArgArray a({});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.save,					false);
	QCOMPARE(pa.logToFile,				false);
	QCOMPARE(pa.hideJASP,				false);
	QCOMPARE(pa.safeGraphics,			false);
	QCOMPARE(pa.newData,				false);
	QCOMPARE(pa.exportPdf,				false);
	QCOMPARE(pa.keepJASPOpenAfterExporting, false);
	QCOMPARE(pa.dontExportResult,		false);
	QCOMPARE(pa.unitTest,				false);
	QCOMPARE(pa.unitTestRecursive,		false);
	QCOMPARE(pa.syncDataFileRecursive,	false);
	QCOMPARE(pa.mainFileIsJaspFile,		false);
	QCOMPARE(pa.timeOut,				10);
	QVERIFY(!pa.mainFilePath.exists());
	QVERIFY(pa.dataFiles.empty());
}

// ── boolean flags ──────────────────────────────────────────────────────────

void TestParsedArguments::testBooleanFlags()
{
	auto check = [](const char* flag, bool ParsedArguments::* member)
	{
		ArgArray a({flag});
		ParsedArguments pa(a.argc, a.argv());
		QVERIFY2(pa.*member, flag);
	};

	check("--save",				&ParsedArguments::save);
	check("--logToFile",		&ParsedArguments::logToFile);
	check("--hide",				&ParsedArguments::hideJASP);
	check("--safeGraphics",		&ParsedArguments::safeGraphics);
	check("--newData",			&ParsedArguments::newData);

#ifdef PRO
	//These flags only exist in PRO builds; elsewhere they are passed on to Qt as unrecognized options.
	check("--exportPdf",		&ParsedArguments::exportPdf);
	check("--keepJASPOpen",		&ParsedArguments::keepJASPOpenAfterExporting);
	check("--dontExportResult",	&ParsedArguments::dontExportResult);
#endif
}

// ── --timeOut=N ────────────────────────────────────────────────────────────

void TestParsedArguments::testTimeoutDefault()
{
	ArgArray a({});
	ParsedArguments pa(a.argc, a.argv());
	QCOMPARE(pa.timeOut, 10);
}

void TestParsedArguments::testTimeoutParsing()
{
	auto check = [](const char* arg, int expected)
	{
		ArgArray a({arg});
		ParsedArguments pa(a.argc, a.argv());
		QCOMPARE(pa.timeOut, expected);
	};

	check("--timeOut=1",  1);
	check("--timeOut=30", 30);
	check("--timeOut=60", 60);
}

void TestParsedArguments::testTimeoutInvalidKeepsDefault()
{
	// Non-numeric value: stoi throws, convertedChars stays 0, timeout unchanged
	ArgArray a({"--timeOut=notanumber"});
	ParsedArguments pa(a.argc, a.argv());
	QCOMPARE(pa.timeOut, 10);
}

// ── positional .jasp file ─────────────────────────────────────────────────

void TestParsedArguments::testJaspFilePositionalArg()
{
	QTemporaryDir dir;
	QVERIFY(dir.isValid());
	QString jaspPath = createTempJaspFile(dir);

	ArgArray a({jaspPath});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.mainFileIsJaspFile, true);
	QCOMPARE(pa.mainFilePath.absoluteFilePath(), QFileInfo(jaspPath).absoluteFilePath());
	QVERIFY(pa.dataFiles.empty());
	QCOMPARE(pa.syncDataFileRecursive, false);
}

// ── .jasp + data files ────────────────────────────────────────────────────

void TestParsedArguments::testJaspFileWithOneDataFile()
{
#ifndef PRO
	QSKIP("A data file following a JASP file is only parsed in PRO builds");
#endif
	QTemporaryDir dir;
	QVERIFY(dir.isValid());
	QString jaspPath = createTempJaspFile(dir);
	QString csvPath  = createTempDataFile(dir);

	ArgArray a({jaspPath, csvPath});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.mainFileIsJaspFile, true);
	QCOMPARE((int)pa.dataFiles.size(), 1);
	QCOMPARE(pa.dataFiles[0].absoluteFilePath(), QFileInfo(csvPath).absoluteFilePath());
	// One data file: syncDataFileRecursive stays false
	QCOMPARE(pa.syncDataFileRecursive, false);
}

void TestParsedArguments::testJaspFileWithMultipleDataFiles()
{
#ifndef PRO
	QSKIP("Data files following a JASP file are only parsed in PRO builds");
#endif
	QTemporaryDir dir;
	QVERIFY(dir.isValid());
	QString jaspPath = createTempJaspFile(dir);
	QString csv1     = createTempDataFile(dir, "data1.csv");
	QString csv2     = createTempDataFile(dir, "data2.csv");

	ArgArray a({jaspPath, csv1, csv2});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.mainFileIsJaspFile, true);
	QCOMPARE((int)pa.dataFiles.size(), 2);
	// Two or more data files → syncDataFileRecursive
	QCOMPARE(pa.syncDataFileRecursive, true);
}

// ── --inputDataDir ─────────────────────────────────────────────────────────

void TestParsedArguments::testInputDataDir()
{
#ifndef PRO
	QSKIP("--inputDataDir is only parsed in PRO builds");
#endif
	QTemporaryDir dataDir;
	QVERIFY(dataDir.isValid());

	ArgArray a({"--inputDataDir", dataDir.path()});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.syncDataFileRecursive, true);
	QCOMPARE(pa.inputDataDir.absoluteFilePath(), QFileInfo(dataDir.path()).absoluteFilePath());
}

// ── --outputDir ────────────────────────────────────────────────────────────

void TestParsedArguments::testOutputDir()
{
#ifndef PRO
	QSKIP("--outputDir is only parsed in PRO builds");
#endif
	QTemporaryDir outDir;
	QVERIFY(outDir.isValid());

	ArgArray a({"--outputDir", outDir.path()});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.outputDir.absoluteFilePath(), QFileInfo(outDir.path()).absoluteFilePath());
}

// ── --unitTest ─────────────────────────────────────────────────────────────

void TestParsedArguments::testUnitTestFlag()
{
	QTemporaryDir dir;
	QVERIFY(dir.isValid());
	QString jaspPath = createTempJaspFile(dir);

	ArgArray a({"--unitTest", jaspPath});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.unitTest, true);
	QCOMPARE(pa.mainFilePath.absoluteFilePath(), QFileInfo(jaspPath).absoluteFilePath());
	// --unitTest doesn't set mainFileIsJaspFile; that's only for positional args
	QCOMPARE(pa.mainFileIsJaspFile, false);
}

// ── --unitTestRecursive ────────────────────────────────────────────────────

void TestParsedArguments::testUnitTestRecursiveFlag()
{
	QTemporaryDir dir;
	QVERIFY(dir.isValid());

	ArgArray a({"--unitTestRecursive", dir.path()});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.unitTestRecursive, true);
	QCOMPARE(pa.mainFilePath.absoluteFilePath(), QFileInfo(dir.path()).absoluteFilePath());
}

// ── combined output flags ─────────────────────────────────────────────────

void TestParsedArguments::testCombinedOutputFlags()
{
#ifndef PRO
	QSKIP("The export/output flags combination is only parsed in PRO builds");
#endif
	QTemporaryDir dir;
	QVERIFY(dir.isValid());
	QString jaspPath = createTempJaspFile(dir);
	QString csvPath  = createTempDataFile(dir);
	QTemporaryDir outDir;
	QVERIFY(outDir.isValid());

	ArgArray a({jaspPath, csvPath,
		"--exportPdf", "--outputDir", outDir.path(),
		"--dontExportResult", "--keepJASPOpen", "--hide"});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.mainFileIsJaspFile,			true);
	QCOMPARE((int)pa.dataFiles.size(),		1);
	QCOMPARE(pa.exportPdf,					true);
	QCOMPARE(pa.dontExportResult,			true);
	QCOMPARE(pa.keepJASPOpenAfterExporting,	true);
	QCOMPARE(pa.hideJASP,					true);
	QCOMPARE(pa.outputDir.absoluteFilePath(), QFileInfo(outDir.path()).absoluteFilePath());
}

// ── several unrelated flags together ──────────────────────────────────────

void TestParsedArguments::testMultipleFlagsIndependent()
{
	ArgArray a({"--save", "--logToFile", "--safeGraphics", "--exportPdf", "--timeOut=5"});
	ParsedArguments pa(a.argc, a.argv());

	QCOMPARE(pa.save,			true);
	QCOMPARE(pa.logToFile,		true);
	QCOMPARE(pa.safeGraphics,	true);
#ifdef PRO
	QCOMPARE(pa.exportPdf,		true);
#endif
	QCOMPARE(pa.timeOut,		5);
	// Everything else stays default
	QCOMPARE(pa.hideJASP,		false);
	QCOMPARE(pa.unitTest,		false);
}


QTEST_MAIN(TestParsedArguments)
