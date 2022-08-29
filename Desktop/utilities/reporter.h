#ifndef REPORTER_H
#define REPORTER_H

#include <QObject>
#include <QDir> 
#include "json/json.h"


class Analysis;

/// This handles the information and logic for checking the results for jaspReport.
/// 
/// This also handles triggering export of the report to the set reporting dir. 
/// It will only be instantiated if JASP is started in reportingmode.
class Reporter : public QObject
{
	Q_OBJECT
public:
	explicit Reporter(QObject *parent, QDir reportingDir);

	static Reporter * reporter();
	
	/// If the jaspfile is a databaseimport it should have synching enabled, if not it should have a datafile that exists.
	/// Otherwise it will just run a single time, and that wouldn't be very useful.
	bool	isJaspFileNotDabaseOrSynching() const;

	Json::Value reportsFromAnalysis(Analysis * a, int & reportsNeeded, int & reportsNeutral);
	bool analysisHasReportNeeded(Analysis * a);
	
public slots:
	void	analysesFinished();	///< Should be called whenever the last noncompleted analysis completes.
	void	onPdfPrintingFinishedHandler(QString pdfPath);
	
private:
	void	exportPdf();
	bool	checkReports();
	void	writeResultsJson();	///< write the entire resultsjson as given to results-webpage to reporting dir. This can later be used for a dashboard
	void	writeReport();
	void	writeReportLog();
	void	exportDashboard();

private:
	QDir					_reportingDir;
	Json::Value				_reports;
	int						_reportsNeeded, ///< How many reports need to be attended to
							_reportsNeutral;///< How many are neutral and can be ignored
	QMetaObject::Connection _pdfConnection;
	QString					_pdfPath;		

	static Reporter		*	_reporter;
};

#endif // REPORTER_H
