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

	void	enableTestMode()			{ runningTestMode = true; }
	bool	testMode()			const	{ return runningTestMode; }

	void	enableSaving()				{ saveAfterRefresh = true; }
	bool	shouldSave()		const	{ return saveAfterRefresh; }

	void	setRefreshCalled()			{ atLeastOneRefreshHappened = true; }
	bool	refreshed()			const	{ return atLeastOneRefreshHappened; }

	void	setExportCalled()			{ resultsExportCalled = true; }
	bool	exportCalled()		const	{ return resultsExportCalled; }

	bool	comparedAlready()	const	{ return ranCompare;	}
	bool	compareSucces()		const	{ return succes;		}

	QString	filePath()			const	{ return _filePath;	}
	void	setFilePath(QString p)		{ _filePath = p;	}

	static	compareResults	*theOne();

private:
	explicit		compareResults() {}

	bool			runningTestMode				= false,
					atLeastOneRefreshHappened	= false,
					resultsExportCalled			= false,
					saveAfterRefresh			= false,
					ranCompare					= false,
					succes						= false;

	QString			originalResultExport		= "",
					refreshedResultExport		= "",
					_filePath					= "";

	static compareResults*	singleton;
};

}

#endif // COMPARERESULTS_H
