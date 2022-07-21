#include "knownissues.h"
#include "utilities/messageforwarder.h"
#include "appinfo.h"
#include "utilities/jsonutilities.h"
#include "log.h"
#include "utilities/appdirs.h"
#include "utils.h"
#include <fstream>


//Check every day?
#define EXPIRATION_TIME_SEC 60 * 60 * 24

// https://www.youtube.com/watch?v=REWeBzGuzCc
KnownIssues * KnownIssues::_knownIssues = nullptr;

KnownIssues::KnownIssues(QObject * parent) : QObject(parent)
{
	assert(!_knownIssues);
	_knownIssues = this;
}

void KnownIssues::loadLocalJson(const std::string & filePath, bool saveIt)
{
	std::ifstream readMe(filePath);

	Json::Value json;
	Json::Reader().parse(readMe, json);

	loadJson(json, saveIt);
}

void KnownIssues::loadJson(const std::string & jsonTxt,	bool saveIt)
{
	if(jsonTxt == "") 
	{
		MessageForwarder::showWarning(tr("Problem loading known issues"), tr("JASP ran into a problem downloading the known issues for this version, it probably could not connect to the server. Don't worry, JASP will work fine it just might not tell you about a few small known issues."));
		return;
	}
	
	Json::Value known;
	if(Json::Reader().parse(jsonTxt, known))	loadJson(known, saveIt);
	else										MessageForwarder::showWarning(tr("Problem loading known issues"), tr("JASP ran into a problem loading the known issues for this version, this isn't necessarily a problem but if it keeps occuring you could contact the JASP team for assistance."));
}


void KnownIssues::loadJson(const Json::Value & json, bool saveIt)
{
	_issues.clear();

	try
	{
		if(!json.isObject()) throw std::runtime_error("expected issues json to be an object");

		const std::string version = AppInfo::version.asString();

		if(json.isMember(version))
			for(		const std::string & module		: json[version]						.getMemberNames())
				for(	const std::string & analysis	: json[version][module]				.getMemberNames())
				{
					const Json::Value & perAnalysis = json[version][module][analysis];

					if(perAnalysis.isObject())	addIssue(module, analysis, perAnalysis);
					if(perAnalysis.isArray())
						for(const Json::Value & entry : perAnalysis)
							addIssue(module, analysis, entry);
				}
	}
	catch(const std::runtime_error & e) { MessageForwarder::showWarning(tr("Problem loading known issues"), tr("JASP ran into a problem ('%1') loading the known issues for this version, this isn't necessarily a problem but if it keeps occuring you could contact the JASP team for assistance.").arg(e.what())); }
	catch(...)							{ MessageForwarder::showWarning(tr("Problem loading known issues"), tr("JASP ran into a problem loading the known issues for this version, this isn't necessarily a problem but if it keeps occuring you could contact the JASP team for assistance.")); }

	emit knownIssuesUpdated();

	if(saveIt)
	{
		std::ofstream saveHere(knownJsonPath());
		saveHere << json;
		saveHere.close();
	}
}

std::string KnownIssues::knownJsonPath() const
{
	return fq(AppDirs::appData()) + "/knownIssues.json";
}

void KnownIssues::loadKnownJson()
{
	loadLocalJson(knownJsonPath(), false);
}

bool KnownIssues::knownJsonExpired() const
{
	std::filesystem::path knownJson = std::filesystem::path(knownJsonPath());

	if(!std::filesystem::exists(knownJson))
		return true;

	long modTime	= Utils::getFileModificationTime(Utils::osPath(knownJson));
	long now		= Utils::currentSeconds();

	return now - modTime > EXPIRATION_TIME_SEC;
}

void KnownIssues::addIssue(const std::string & module, const std::string & analysis, const Json::Value & issueJson)
{
	issue newIssue;

	newIssue.info		= issueJson["info"].asString();
	Json::Value options = issueJson.get("options", Json::arrayValue);

	switch(options.type())
	{
	case Json::stringValue:	newIssue.options.insert(options.asString());						break;
	case Json::arrayValue:
	{
		stringvec vec = JsonUtilities::jsonStringArrayToVec(options);
		newIssue.options = stringset(vec.begin(), vec.end());
		break;
	}
	default:				Log::log() << "KnownIssues::addIssue got unexpected type for \"options\", so ignoring it." << std::endl;
	}

	_issues[module][analysis].push_back(newIssue);
}

bool KnownIssues::hasIssues(const std::string & module, const std::string & analysis)
{
	return _issues.count(module) > 0 && _issues[module].count(analysis) > 0;
}

bool KnownIssues::hasIssues(const std::string & module, const std::string & analysis, const std::string & option)
{
	if(!hasIssues(module, analysis)) return false;

	for(const issue & anIssue : _issues[module][analysis])
		if(anIssue.options.count(option) > 0)
			return true;

	return false;
}

std::string KnownIssues::issuesForAnalysis(const std::string & module, const std::string & analysis)
{
	if(!hasIssues(module, analysis)) return "";

	std::stringstream out;

	out << "<ul>";

	for(const issue & anIssue : _issues[module][analysis])
		out << "<li>" << anIssue.info << "</li>\n";

	out << "</ul>";

	return out.str();
}

bool KnownIssues::downloadNeededOrLoad()
{
	if(knownJsonExpired())
		return true;

	loadKnownJson();
	return false;
}
