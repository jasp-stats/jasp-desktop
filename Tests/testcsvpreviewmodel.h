#include <QtTest>
#include <QDebug>


class TestCsvPreviewModel : public QObject
{
    Q_OBJECT
	
private slots:
    void testCsvParsing();
    void testDifferentDelimiters();
};
