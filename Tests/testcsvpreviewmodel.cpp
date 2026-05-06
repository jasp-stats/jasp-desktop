#include "testcsvpreviewmodel.h"
#include "utilities/csvpreviewmodel.h"


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
	QCOMPARE(model.data(model.index(1, 1), Qt::DisplayRole).toString(), QString("\"1,234\""));
	QCOMPARE(model.data(model.index(1, 2), Qt::DisplayRole).toString(), QString("1.2"));

    // Tab delimiter
    QString tabData = "Col1\tCol2\tCol3\nVal1\tVal2\tVal3";
    model.preparePreview(tabData.toStdString().c_str(), '\t');
    QCOMPARE(model.columnCount(), 3);
    QCOMPARE(model.data(model.index(0, 2), Qt::DisplayRole).toString(), QString("Col3"));
}


QTEST_MAIN(TestCsvPreviewModel)
