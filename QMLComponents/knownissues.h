#ifndef KNOWNISSUES_H
#define KNOWNISSUES_H

#include <QObject>
#include <json/json.h>
#include "qutils.h"
#include "stringutils.h"
#include "version.h"
///
/// This class stores information per module and analysis that is pulled from the server occasionally (in JaspVersionChecker)
/// When the user adds an analysis it checks whether we have found a serious issue and posted it to our server. Which allows us to show a message in the qml form
/// Mentioning the problem and that we are working on it. 
/// This way people can be informed of problems with an analysis and they won't try to publish faulty results (in the worst case).
class KnownIssues : public QObject
{
	Q_OBJECT

public:
	struct issue
	{
		std::string		info;
		stringset		options;
	};

	typedef std::map<std::string, std::vector<issue>>			issuesPerAnalysis;
	typedef std::map<Version, issuesPerAnalysis>				issuesPerVersion;
	typedef std::map<std::string, issuesPerVersion>				issuesPerModule;

	explicit KnownIssues(QObject *parent = nullptr);
	~KnownIssues() override { if(_knownIssues == this) _knownIssues = nullptr; } ///< the singleton must not dangle: a second Analyses constructing its own would assert (see FileMenu::_singleton)

	static KnownIssues * issues() { return _knownIssues; }

	bool downloadNeededOrLoad();

	void loadJson(const Json::Value & json,		bool saveIt);
	void loadJson(const std::string & jsonTxt,	bool saveIt);
	void loadJson(const QString     & jsonTxt,	bool saveIt) { loadJson(fq(jsonTxt), saveIt); }

	bool				hasIssues(			const std::string & module, const Version & version, const std::string & analysis);
	bool				hasIssues(			const std::string & module, const Version & version, const std::string & analysis, const std::string & option);
	std::string			issuesForAnalysis(	const std::string & module, const Version & version, const std::string & analysis);

	bool				hasIssues(			const QString     & module, const Version & version, const QString     & analysis)								{ return hasIssues(				fq(module), version, fq(analysis)				); }
	bool				hasIssues(			const QString     & module, const Version & version, const QString     & analysis, const QString & option)		{ return hasIssues(				fq(module), version, fq(analysis), fq(option)	); }
	QString				issuesForAnalysis(	const QString	  & module, const Version & version, const QString	  & analysis)								{ return tq(issuesForAnalysis(	fq(module), version, fq(analysis))				); }

	const std::vector<issue> &	getIssues(	const std::string & module, const Version & version, const std::string & analysis)								{ return _issues[module][version][analysis];	}
	const std::vector<issue> &	getIssues(	const QString     & module, const Version & version, const QString     & analysis)								{ return getIssues(fq(module), version, fq(analysis));			}

signals:
	void knownIssuesUpdated();

private:
	bool		knownJsonExpired()	const;
	std::string knownJsonPath()		const;
	void		loadKnownJson();

	void		loadLocalJson(	const std::string & filePath,	bool saveIt);
	void		addIssue(		const std::string & module,		const Version & version, const std::string & analysis, const Json::Value & issue);

private:
	issuesPerModule			_issues;
	static KnownIssues *	_knownIssues;
};

#endif // KNOWNISSUES_H
