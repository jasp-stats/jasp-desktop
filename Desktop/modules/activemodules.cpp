#include "activemodules.h"
#include "utilities/appdirs.h"
#include <QString>
#include <QDir>

const std::set<std::string> ActiveModules::defaultExtraModules =
	{"jaspAcceptanceSampling",
	  "jaspAudit",
	  "jaspBain",
	  "jaspBFF",
	  "jaspBfpack",
	  "jaspBsts",
	  "jaspCircular",
	  "jaspCochrane",
	  "jaspDistributions",
	  "jaspEquivalenceTTests",
	  "jaspEsci",
	  "jaspJags",
	  "jaspLearnBayes",
	  "jaspLearnStats",
	  "jaspMachineLearning",
	  "jaspMetaAnalysis",
	  "jaspNetwork",
	  "jaspPower",
	  "jaspPredictiveAnalytics",
	  "jaspProcess",
	  "jaspProphet",
	  "jaspQualityControl",
	  "jaspReliability",
	  "jaspRobustTTests",
	  "jaspSem",
	  "jaspSurvival",
	  "jaspSummaryStatistics",
	  "jaspTimeSeries",
	  "jaspVisualModeling"};

QStringList getShippedModules()
{
	auto dir = QDir(AppDirs::bundledModulesDir());
	if(!dir.exists())
		return {};

	return dir.entryList({"jasp*"}, QDir::Dirs);
}

std::vector<std::string> ActiveModules::getActiveCommonModules()
{
	std::vector<std::string> result;
	for(auto& module : getShippedModules()) {
		if(defaultExtraModules.find(module.toStdString()) == defaultExtraModules.end())
			result.push_back(module.toStdString());
	}
	return result;
}

std::vector<std::string> ActiveModules::getActiveExtraModules()
{
	std::vector<std::string> result;
	for(auto& module : getShippedModules()) {
		if(!(defaultExtraModules.find(module.toStdString()) == defaultExtraModules.end()))
			result.push_back(module.toStdString());
	}
	return result;
}


