//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "boundqmlfactorslist.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"

#include <QQmlProperty>
#include <QQuickItem>

using namespace std;

BoundQMLFactorsList::BoundQMLFactorsList(QQuickItem *item, AnalysisQMLForm *form)
	: QMLItem(item, form)
	, QMLListView(item, form)
	, BoundQMLItem(item, form)
{
	_factorsModel = new ListModelFactors(this);
	setTermsAreNotVariables();
	
	QQuickItem::connect(_item, SIGNAL(itemChanged(int, QVariant)), _factorsModel, SLOT(itemChanged(int, QVariant)));	
	QQuickItem::connect(_item, SIGNAL(itemRemoved(int)), _factorsModel, SLOT(itemRemoved(int)));		
}

void BoundQMLFactorsList::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	vector<pair<string, vector<string> > > factors;
	vector<Options*> allOptions = _boundTo->value();
	
	for (const Options* options : allOptions)
	{
		OptionString *factorNameOption = static_cast<OptionString *>(options->get("name"));
		OptionVariables *factorLevelsOption = static_cast<OptionVariables *>(options->get("levels"));
		
		factors.push_back(make_pair(factorNameOption->value(), factorLevelsOption->variables()));
	}
	
	_factorsModel->initFactors(factors);
}

void BoundQMLFactorsList::unbind()
{
}

Option* BoundQMLFactorsList::createOption()
{
	OptionsTable* optionsTable = new OptionsTable();
	
	Options* options = new Options();
	options->add("name", new OptionString("RM Factor 1"));
	OptionVariables* levels = new OptionVariables();
	std::vector<std::string> firstLevels;
	firstLevels.push_back("Level 1");
	firstLevels.push_back("Level 2");
	levels->setValue(firstLevels);
	options->add("levels", levels);
	
	std::vector<Options*> allOptions;
	allOptions.push_back(options);
	optionsTable->connectOptions(allOptions);
	
	return optionsTable;
}

void BoundQMLFactorsList::modelChangedHandler()
{
	const vector<pair<string, vector<string> > > &factors = _factorsModel->getFactors();
	vector<Options *> allOptions;
	
	for (const auto &factor : factors)
	{
		Options* options = new Options();
		options->add("name", new OptionString(factor.first));
		OptionVariables* levelVariables = new OptionVariables();
		vector<string> levels;
		for (const string &level : factor.second)
			levels.push_back(level);
		levelVariables->setValue(levels);
		options->add("levels", levelVariables);
		allOptions.push_back(options);
	}
	
	_boundTo->setValue(allOptions);	
}
