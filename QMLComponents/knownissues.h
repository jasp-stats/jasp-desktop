#ifndef KNOWNISSUES_H
#define KNOWNISSUES_H

#include <QObject>
#include <json/json.h>
#include "utilities/qutils.h"
#include "stringutils.h"

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
		std::string	info;
		stringset	options;
	};

	typedef std::map<std::string, std::vector<issue>>	issuesPerAnalysis;
	typedef std::map<std::string, issuesPerAnalysis>	issuesPerModule;

	explicit KnownIssues(QObject *parent = nullptr);

	static KnownIssues * issues() { return _knownIssues; }

	bool downloadNeededOrLoad();

	void loadJson(const Json::Value & json,		bool saveIt);
	void loadJson(const std::string & jsonTxt,	bool saveIt);
	void loadJson(const QString     & jsonTxt,	bool saveIt) { loadJson(fq(jsonTxt), saveIt); }

	bool				hasIssues(			const std::string & module, const std::string & analysis);
	bool				hasIssues(			const std::string & module, const std::string & analysis, const std::string & option);
	std::string			issuesForAnalysis(	const std::string & module, const std::string & analysis);

	bool				hasIssues(			const QString     & module, const QString     & analysis)								{ return hasIssues(				fq(module), fq(analysis)			 ); }
	bool				hasIssues(			const QString     & module, const QString     & analysis, const QString     & option)	{ return hasIssues(				fq(module), fq(analysis), fq(option) ); }
	QString				issuesForAnalysis(	const QString	  & module, const QString	  & analysis)								{ return tq(issuesForAnalysis(	fq(module), fq(analysis)			)); }

	const std::vector<issue> &	getIssues(	const std::string & module, const std::string & analysis)								{ return _issues[module][analysis]; }
	const std::vector<issue> &	getIssues(	const QString     & module, const QString     & analysis)								{ return getIssues(fq(module), fq(analysis)); }

signals:
	void knownIssuesUpdated();

private:
	bool		knownJsonExpired()	const;
	std::string knownJsonPath()		const;
	void		loadKnownJson();

	void		loadLocalJson(	const std::string & filePath,	bool saveIt);
	void		addIssue(		const std::string & module,		const std::string & analysis, const Json::Value & issue);

private:
	issuesPerModule			_issues;
	static KnownIssues *	_knownIssues;
};

#endif // KNOWNISSUES_H
