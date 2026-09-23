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
#include <QtTest>
#include <QDebug>

class LanguageModel;

class TestCsvPreviewModel : public QObject
{
    Q_OBJECT
	
private slots:
	void initTestCase();
    void testCsvParsing();
    void testDifferentDelimiters();
	void testImportLocale();
	void testImportLocaleIsHandedToTheImporter();
	void testLocaleFallsBackOnTheInterface();
	void testMoreLanguagesWidensTheLanguageList();

private:
	LanguageModel * _languageModel = nullptr;	///< CsvPreviewModel gets its languages and territories from here
};
