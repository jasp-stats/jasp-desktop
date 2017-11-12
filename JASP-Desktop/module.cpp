#include "module.h"
#include "QSettings"
#include <QDebug>

using namespace std;

map<QString, Module> Module::AllModules = {
	{"Common", Module("Common")},
#ifndef __linux__
	{"SEM", Module("SEM", 1)},
#endif
	{ "ReinforcementLearning", Module("ReinforcementLearning", "Reinforcement Learning", 2, false)},
	{"SummaryStats", Module("SummaryStats", "Summary Stats", 3)},
	{"MetaAnalysis", Module("MetaAnalysis", "Meta Analysis", 4)},
	{"Network", Module("Network", 5)},
///// ribbon tab number: 6
};


Module::Module(QString name, int ribbonIndex, bool released) : _name(name), _displayName(name)
{
	_ribbonIndex = ribbonIndex;
	_released = released;
}

Module::Module(QString name, QString displayName, int ribbonIndex, bool released) : _name(name), _displayName(displayName)
{
	_ribbonIndex = ribbonIndex;
	_released = released;
}

bool Module::isModuleName(QString name)
{
	return AllModules.find(name) != AllModules.end();
}

const Module &Module::getModule(QString name)
{
	return AllModules.at(name);
}
