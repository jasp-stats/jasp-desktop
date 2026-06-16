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
	void	testChangeLabelValueTwice();
	void    testReverseLabels();
    void    testReverseNumericals();
	void	testShadowDisplay();
	void	testValueEqualsDisplayStorage();
	void	testSequentialValueChanges();
	void	testEmptyValueLabel();
	void	testNumericToStringConversion();
	void	testStringToNumericConversion();
	void	testBatchOperationsWithFilters();
	//void	testUndoRedoAfterLabelChanges();
	
private:
	DataSetPackage		*	_pkg		= nullptr;
	DataSet				*	_data		= nullptr;
	Importer			*	_importer	= nullptr;
	const char			*	_debugCsv	= "";
};


#endif // TESTDEBUGDATA_H
