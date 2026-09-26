#include <QTest>

class DataSetPackage;
class Importer;

class TestAll: public QObject
{
    Q_OBJECT
	
private slots:
    void    initTestCase();
    void    init();
	void	cleanup();
	void    testDataImport();
	void	testDataImport_data();
	void	testJaspDataImport();
	void	testJaspDataImport_data();
	void	testJaspRoundRobin_data();
	void	testJaspRoundRobin();
	void	testCancelledImportLeavesTheModelUsable();
	void	testDatabaseImportNulls();
	void	testCsvImportLocale();
	void	testCsvImportLocaleColumnWidth();
	void	testNumbersGroupedWithSpaces();
	void	testCsvImportNumbers_data();
	void	testCsvImportNumbers();
	void	testCsvSyncNumbers_data();
	void	testCsvSyncNumbers();
	void	testCsvSyncTextAndNumbersStayApart();
	void	testFailedLoadOrSyncForgetsCsvChoices();
	void	testOdsImportLocale();
	void	testMinitabImportLocale();
	void	testDataSetsTableUpgrade();
	void	testSavLabels();
	void	testFilterLabels();
	void	testRpcScriptCallerLeavesTheAgentViewAlone();
	void	testRpcDispatchWhenFreeWaitsItsTurn();
	void	testRpcServerQueuesBusyCalls();
	void	testRpcScriptServerNeedsItsToken();
	void	testPythonModuleCallsJasp();
	void	testPythonScriptRunner();

private:
	DataSetPackage		*	_pkg		= nullptr;
	Importer			*	_importer	= nullptr;

	// keepMissingColsWhenSyncing: pins the current semantics, including the fact that the kept columns
	// accumulate over the syncs of one session (see the --keepMissingColsWhenSyncing help text).
	void	testSyncKeepMissingColumns();

};
