#ifndef TESTDEBUGDATA_H
#define TESTDEBUGDATA_H

#include <QTest>

class DataSetPackage;
class Importer;
class DataSet;

class TestDebugData: public QObject
{
    Q_OBJECT
private slots:
    void    init();
	void	cleanup();
	void    initTestCase();
	void    testColumnStuff();
	void	testEmptyValues();
	void	testChangeLabel();
	void    testReverseLabels();
    void    testReverseNumericals();
	

private:
	DataSetPackage		*	_pkg		= nullptr;
	DataSet				*	_data		= nullptr;
	Importer			*	_importer	= nullptr;
	const char			*	_debugCsv	= "";
};


#endif // TESTDEBUGDATA_H
