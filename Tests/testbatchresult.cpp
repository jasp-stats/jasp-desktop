#include <QtTest>
#include "../Desktop/batchresult.h"
#include "../QMLComponents/utilities/messageforwarder.h"

class TestBatchResult : public QObject
{
	Q_OBJECT
private slots:
	void nestedAnalysisErrors()
	{
		Json::Value result(Json::objectValue);
		result[".meta"][0]["name"] = "model";
		result["model"]["collection"]["table"]["error"]["errorMessage"] = "No observations remain";
		BatchResult report;
		report.collect(result, "Regression");
		QCOMPARE(report.errors, QStringList{"Regression: No observations remain"});
	}
	void tableDataIsNotDiagnostics()
	{
		Json::Value result(Json::objectValue);
		result["data"][0]["errorMessage"] = "user data";
		result["footnotes"][0]["text"] = "A normal table note";
		BatchResult report;
		report.collect(result, "Regression");
		QVERIFY(report.errors.isEmpty());
		QVERIFY(report.warnings.isEmpty());
	}
	void errorWithoutMessage()
	{
		Json::Value result(Json::objectValue);
		result[".meta"][0]["name"] = "model";
		result["model"]["error"]["errorMessage"] = "";
		BatchResult report;
		report.collect(result, "Regression");
		QCOMPARE(report.errors, QStringList{"Regression: Analysis failed"});
	}
	void warningsAreReportedWithoutDialogs()
	{
		BatchResult report;
		MessageForwarder::setWarningHandler([&](const QString & title, const QString & message, bool error) {
			if (error) report.addError(title + ": " + message);
			else report.addWarning(title + ": " + message);
		});
		MessageForwarder::showWarning("Import", "Some values were missing");
		MessageForwarder::showWarning("Import", "Failed", QMessageBox::Critical);
		MessageForwarder::setWarningHandler({});
		QCOMPARE(report.warnings, QStringList{"Import: Some values were missing"});
		QCOMPARE(report.errors, QStringList{"Import: Failed"});
	}
	void warningAndErrorRoundTrip()
	{
		BatchResult report, received;
		report.addError("Import failed\nMore detail");
		report.addWarning(QString::fromUtf8("Warning: český"));
		report.addWarning(QString::fromUtf8("Warning: český"));
		QVERIFY(received.read(report.serialize()));
		QCOMPARE(received.errors, report.errors);
		QCOMPARE(received.warnings.size(), 1);
		QCOMPARE(received.warnings, report.warnings);
		QVERIFY(!received.read("JASP_BATCH_RESULT broken json"));
		QVERIFY(!received.read("ordinary log output"));
	}
};

QTEST_GUILESS_MAIN(TestBatchResult)
#include "testbatchresult.moc"
