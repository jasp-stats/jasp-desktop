#ifndef TESTENGINE_H
#define TESTENGINE_H


#include <QTest>

class EngineRepresentation;
class DataSetPackage;
class EngineSync;
class Importer;
class DataSet;
class Filter;
class VariableInfo;

class TestEngine: public QObject
{
    Q_OBJECT
private slots:
    void    init();
	void	cleanup();
    void    testFilters();
    void    initTestCase();
    void    testComputedColumns();
	void    testComputedColumnCascade();
	void    testComputedDataSet();
    void    testVariableInfoPerFilter();
	void	testScriptConstructorFuzz();

private:
	EngineSync				*	_engines	= nullptr;
	EngineRepresentation	*	_engineRep	= nullptr;
	DataSetPackage			*	_pkg		= nullptr;
	DataSet					*	_data		= nullptr;
	Importer				*	_importer	= nullptr;
	const char				*	_debugCsv	= "";
};

#endif // TESTENGINE_H
