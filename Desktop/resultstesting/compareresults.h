#ifndef COMPARERESULTS_H
#define COMPARERESULTS_H

#include <QString>
#include "resultscomparetable.h"

namespace resultXmlCompare
{

///
/// Main class for running JASP-file-unit-tests
/// The basis here is that each jasp-file contains the output as a html page, we can parse it and compare it with what we get when we rerun it.
/// That way we can be sure that the output is still the same in a newer version of JASP/module
/// It doesn however only look at tables and for the plots maybe at the title but not at the image.
class compareResults
{
public:
	bool	compare();
	bool	compare(const QString & resultOld, const QString & resultNew);

	result	convertXmltoResultStruct(const QString & resultXml);

	void	sanitizeHtml(QString & result);

	void	setOriginalResult(QString result);
	void	setRefreshResult(QString result);

	bool	checkForAnalysisError();

	bool	analysisHadError()	const	{ return _analysisHadError; }

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
					succes						= false,
					_analysisHadError			= false;

	QString			originalResultExport		= "",
					refreshedResultExport		= "",
					_filePath					= "";

	static compareResults*	singleton;
};

}

#endif // COMPARERESULTS_H
