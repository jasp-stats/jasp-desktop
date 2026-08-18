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
#include "utilities/csvpreviewmodel.h"


void TestCsvPreviewModel::testCsvParsing()
{
    CsvPreviewModel model;
    
 	QString rawData = "Col1,Col2,Col3\nVal1,Val2,Val3\n1.2,1.20,1.344";
    model.preparePreview(rawData, ',');

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
    model.preparePreview(rawData, ';');
    QCOMPARE(model.rowCount(), 3);
    QCOMPARE(model.columnCount(), 1);
    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1,Col2,Col3"));

    // Test delimiter change back to correct one
    model.preparePreview(rawData, ',');
    QCOMPARE(model.columnCount(), 3);
    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1"));
}

void TestCsvPreviewModel::testDifferentDelimiters()
{
    CsvPreviewModel model;

    // Semicolon delimiter
 	QString semicolonData = "Col1;Col2;Col3\n1,2;1,234;1.2";
    model.preparePreview(semicolonData, ';');
    QCOMPARE(model.columnCount(), 3);
	QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("\"1,2\""));
	QCOMPARE(model.data(model.index(1, 1), Qt::DisplayRole).toString(), QString("\"1,234\""));
	QCOMPARE(model.data(model.index(1, 2), Qt::DisplayRole).toString(), QString("1.2"));

    // Tab delimiter
    QString tabData = "Col1\tCol2\tCol3\nVal1\tVal2\tVal3";
    model.preparePreview(tabData, '\t');
    QCOMPARE(model.columnCount(), 3);
    QCOMPARE(model.data(model.index(0, 2), Qt::DisplayRole).toString(), QString("Col3"));
}

void TestCsvPreviewModel::testComplexCsvParsing()
{
    CsvPreviewModel model;
    
    QString rawData = 
        "ID,Names,Gender,Age,Add.,Tel.,Text\n"
        "1,One,M,32,ABBA,1234567891,\"test\"\"Quote\"\"\"\n"
        "2,Two,M,21,,1234567890,\"test：\nnewlines\"\n"
        "3,Three,F,18,,,\"this,is,text\"";

    model.preparePreview(rawData, ',');

    QCOMPARE(model.rowCount(), 4);    // header + 3 data rows
    QCOMPARE(model.columnCount(), 7);

    // Header row
    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("ID"));
    QCOMPARE(model.data(model.index(0, 1), Qt::DisplayRole).toString(), QString("Names"));
    QCOMPARE(model.data(model.index(0, 2), Qt::DisplayRole).toString(), QString("Gender"));
    QCOMPARE(model.data(model.index(0, 3), Qt::DisplayRole).toString(), QString("Age"));
    QCOMPARE(model.data(model.index(0, 4), Qt::DisplayRole).toString(), QString("Add."));
    QCOMPARE(model.data(model.index(0, 5), Qt::DisplayRole).toString(), QString("Tel."));
    QCOMPARE(model.data(model.index(0, 6), Qt::DisplayRole).toString(), QString("Text"));

    QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("1"));
    QCOMPARE(model.data(model.index(1, 1), Qt::DisplayRole).toString(), QString("\"One\""));
    QCOMPARE(model.data(model.index(1, 2), Qt::DisplayRole).toString(), QString("\"M\""));
    QCOMPARE(model.data(model.index(1, 3), Qt::DisplayRole).toString(), QString("32"));
    QCOMPARE(model.data(model.index(1, 4), Qt::DisplayRole).toString(), QString("\"ABBA\""));
    QCOMPARE(model.data(model.index(1, 5), Qt::DisplayRole).toString(), QString("1234567891"));
    // Expected: test"Quote" (inner double quote from escaped "")
    // The preview adds outer quotes for non-numeric strings, so final display: "test"Quote""
    // In QString literal: "\"test\"Quote\"\""
    QCOMPARE(model.data(model.index(1, 6), Qt::DisplayRole).toString(), QString("\"test\"Quote\"\""));

    QCOMPARE(model.data(model.index(2, 0), Qt::DisplayRole).toString(), QString("2"));
    QCOMPARE(model.data(model.index(2, 1), Qt::DisplayRole).toString(), QString("\"Two\""));
    QCOMPARE(model.data(model.index(2, 2), Qt::DisplayRole).toString(), QString("\"M\""));
    QCOMPARE(model.data(model.index(2, 3), Qt::DisplayRole).toString(), QString("21"));
    QCOMPARE(model.data(model.index(2, 4), Qt::DisplayRole).toString(), QString(""));   // empty field
    QCOMPARE(model.data(model.index(2, 5), Qt::DisplayRole).toString(), QString("1234567890"));
    // Newline replaced with space: "test： newlines", plus outer quotes: "\"test： newlines\""
    QCOMPARE(model.data(model.index(2, 6), Qt::DisplayRole).toString(), QString("\"test： newlines\""));

    QCOMPARE(model.data(model.index(3, 0), Qt::DisplayRole).toString(), QString("3"));
    QCOMPARE(model.data(model.index(3, 1), Qt::DisplayRole).toString(), QString("\"Three\""));
    QCOMPARE(model.data(model.index(3, 2), Qt::DisplayRole).toString(), QString("\"F\""));
    QCOMPARE(model.data(model.index(3, 3), Qt::DisplayRole).toString(), QString("18"));
    QCOMPARE(model.data(model.index(3, 4), Qt::DisplayRole).toString(), QString(""));
    QCOMPARE(model.data(model.index(3, 5), Qt::DisplayRole).toString(), QString(""));
    // Field contains commas but is quoted as a whole, preview adds outer quotes
    QCOMPARE(model.data(model.index(3, 6), Qt::DisplayRole).toString(), QString("\"this,is,text\""));
}

void TestCsvPreviewModel::testNoTrailingNewline()
{
    CsvPreviewModel model;

    QString rawData = "Col1,Col2,Col3\nVal1,Val2,Val3\n1.2,1.20,1.344";
    model.preparePreview(rawData, ',');

    QCOMPARE(model.rowCount(), 3);
    QCOMPARE(model.columnCount(), 3);

    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1"));
    QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("\"Val1\""));
    QCOMPARE(model.data(model.index(2, 0), Qt::DisplayRole).toString(), QString("1.2"));
}

void TestCsvPreviewModel::testCRLFLineEndings()
{
    CsvPreviewModel model;

    QString rawData = "Col1,Col2\r\nVal1,Val2\r\n1.2,1.20";
    model.preparePreview(rawData, ',');

    QCOMPARE(model.rowCount(), 3);
    QCOMPARE(model.columnCount(), 2);

    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1"));
    QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("\"Val1\""));
    QCOMPARE(model.data(model.index(2, 0), Qt::DisplayRole).toString(), QString("1.2"));

    // Fields should NOT have trailing spaces from \r replacement
    QCOMPARE(model.data(model.index(2, 1), Qt::DisplayRole).toString(), QString("1.2"));
}

void TestCsvPreviewModel::testCRLineEndings()
{
    CsvPreviewModel model;

    QString rawData = "Col1,Col2\rVal1,Val2\r1.2,1.20";
    model.preparePreview(rawData, ',');

    QCOMPARE(model.rowCount(), 3);
    QCOMPARE(model.columnCount(), 2);

    QCOMPARE(model.data(model.index(0, 0), Qt::DisplayRole).toString(), QString("Col1"));
    QCOMPARE(model.data(model.index(1, 0), Qt::DisplayRole).toString(), QString("\"Val1\""));
    QCOMPARE(model.data(model.index(2, 0), Qt::DisplayRole).toString(), QString("1.2"));
}


QTEST_MAIN(TestCsvPreviewModel)
