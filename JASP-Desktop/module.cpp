//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "module.h"
#include <QDebug>

using namespace std;

map<QString, Module> Module::AllModules = {
	{"Common", Module("Common")},
	{"SEM", Module("SEM", 1)},
	{ "ReinforcementLearning", Module("ReinforcementLearning", "Reinforcement Learning", 2, false)},
	{"SummaryStats", Module("SummaryStats", "Summary Stats", 3)},
	{"MetaAnalysis", Module("MetaAnalysis", "Meta Analysis", 4)},
	{"Network", Module("Network", 5)},
	{"BAIN", Module("BAIN", 6)},
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
