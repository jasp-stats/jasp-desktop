#ifndef COMPARERESULTS_H
#define COMPARERESULTS_H

#include <QString>
#include "resultscomparetable.h"

namespace resultXmlCompare
{

class compareResults
{
public:
	bool	compare();
	bool	compare(const QString & resultOld, const QString & resultNew);

	result	convertXmltoResultStruct(const QString & resultXml);

	void	sanitizeHtml(QString & result);

	void	setOriginalResult(QString result);
	void	setRefreshResult(QString result);

	void	enableTestMode()	{ runningTestMode = true; }
	bool	testMode()			{ return runningTestMode; }

	void	setRefreshCalled()	{ atLeastOneRefreshHappened = true; }
	bool	refreshed()			{ return atLeastOneRefreshHappened; }

	void	setExportCalled()	{ resultsExportCalled = true; }
	bool	exportCalled()		{ return resultsExportCalled; }

	static	compareResults	*theOne();

private:
	explicit		compareResults() {}

	bool			runningTestMode				= false,
					atLeastOneRefreshHappened	= false,
					resultsExportCalled			= false;
	QString			originalResultExport		= "",
					refreshedResultExport		= "";

	static compareResults*	singleton;
};

}

#endif // COMPARERESULTS_H
