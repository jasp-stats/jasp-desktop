#ifndef REPORTER_H
#define REPORTER_H

#include <QObject>
#include <QDir> 
#include "json/json.h"

/// This handles the information and logic for checking the results for jaspReport.
/// 
/// This also handles triggering export of the report to the set reporting dir. 
/// It will only be instantiated if JASP is started in reportingmode.
class Reporter : public QObject
{
	Q_OBJECT
public:
	explicit Reporter(QObject *parent, QDir reportingDir);
	
	/// If the jaspfile is a databaseimport it should have synching enabled, if not it should have a datafile that exists.
	/// Otherwise it will just run a single time, and that wouldn't be very useful.
	bool	isJaspFileNotDabaseOrSynching() const;
	
public slots:
	void	analysesFinished();	///< Should be called whenever the last noncompleted analysis completes.
	void	onPdfPrintingFinishedHandler(QString pdfPath);
	
private:
	void	exportPdf();
	bool	checkReports();
	void	writeResultsJson();	///< write the entire resultsjson as given to results-webpage to reporting dir. This can later be used for a dashboard
	void	writeReport();

private:
	QDir					_reportingDir;
	Json::Value				_reports;
	QMetaObject::Connection _pdfConnection;
	QString					_pdfPath;		
};

#endif // REPORTER_H
