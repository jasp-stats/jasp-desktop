#ifndef UPGRADER_H
#define UPGRADER_H

#include <set>
#include <QObject>
#include "upgradestep.h"


class Analysis;

namespace Modules
{

class DynamicModule;

class Upgrader : public QObject
{
	Q_OBJECT
	typedef std::vector<UpgradeStep*>					Steps;
	typedef std::map<std::string, Steps>				StepsPerMod;
	typedef std::map<std::string, const UpgradeStep *>	StepPerName;
	typedef std::map<Version, StepPerName>				StepsPerVersion;
	typedef std::map<std::string, StepsPerVersion>		StepSearch;

	struct StepTaken
	{
		std::string module,
					name;
		Version		version;
		bool		operator<(const StepTaken & other) const;
	};
	typedef std::set<StepTaken> StepsTaken;

public:
	explicit Upgrader(QObject *parent = nullptr);
	static Upgrader * upgrader() { return _singleton; }
	~Upgrader();

	void processUpgradeJson(const std::string & module, const Json::Value & upgrades, DynamicModule * dynMod = nullptr);
	void removeStepsOfModule(const std::string & module);
	void loadOldSchoolUpgrades();

	bool upgradeAnalysisData(Json::Value & analysisData, UpgradeMsgs & msgs) const;

private:
	static Upgrader * _singleton;
	void _upgradeOptionsFromJaspFile(Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken) const;

	StepsPerMod		_allSteps; //vectors of steps organized by name of originating module
	StepSearch		_searcher; //a map organized by from-module with maps organized as a step per version.

};

}
#endif // UPGRADER_H
