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

#include "repeatedmeasuresfactorslistbase.h"
#include "analysis/options/optionencodablestring.h"
#include "analysis/options/optionvariables.h"
#include "analysis/jaspcontrol.h"

using namespace std;

RepeatedMeasuresFactorsListBase::RepeatedMeasuresFactorsListBase(QQuickItem *parent)
	: JASPListControl(parent)
{
	_controlType = ControlType::RepeatedMeasuresFactorsList;
}

void RepeatedMeasuresFactorsListBase::setUpModel()
{
	_factorsModel = new ListModelRepeatedMeasuresFactors(this);
	JASPListControl::setUpModel();
}

void RepeatedMeasuresFactorsListBase::setUp()
{
	JASPListControl::setUp();

	QQuickItem::connect(this, SIGNAL(itemChanged(int, QVariant)), _factorsModel, SLOT(itemChanged(int, QVariant)));
	QQuickItem::connect(this, SIGNAL(itemRemoved(int)), _factorsModel, SLOT(itemRemoved(int)));

}

void RepeatedMeasuresFactorsListBase::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	
	vector<pair<string, vector<string> > > factors;
	vector<Options*> allOptions = _boundTo->value();
	
	for (const Options* options : allOptions)
	{
		OptionEncodableString	* factorNameOption		= static_cast<OptionEncodableString *>(options->get("name"));
		OptionVariables			* factorLevelsOption	= static_cast<OptionVariables		*>(options->get("levels"));
		
		factors.push_back(make_pair(factorNameOption->value(), factorLevelsOption->variables()));
	}
	
	_factorsModel->initFactors(factors);
}

Option* RepeatedMeasuresFactorsListBase::createOption()
{
	
	Options* templote =				new Options();
	templote->add("name",			new OptionEncodableString());
	templote->add("levels",			new OptionVariables(true));
	
	OptionsTable* optionsTable =	new OptionsTable(templote);

	OptionVariables* levels =		new OptionVariables(true);
	std::vector<std::string> firstLevels;
	firstLevels.push_back(tr("Level %1").arg(1).toStdString());
	firstLevels.push_back(tr("Level %1").arg(2).toStdString());
	levels->setValue(firstLevels);

	Options* options =		new Options();
	options->add("name",	new OptionEncodableString(tr("RM Factor %1").arg(1).toStdString()));
	options->add("levels", levels);
	
	std::vector<Options*> allOptions;
	allOptions.push_back(options);
	optionsTable->connectOptions(allOptions);
	
	return optionsTable;
}

bool RepeatedMeasuresFactorsListBase::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool RepeatedMeasuresFactorsListBase::isJsonValid(const Json::Value &optionValue)
{
	bool valid = optionValue.type() == Json::arrayValue;

	if (valid)
	{
		for (uint i = 0; i < optionValue.size(); i++)
		{
			const Json::Value& value = optionValue[i];
			valid = value.type() == Json::objectValue;
			if (valid)
			{
				const Json::Value & nameOption		= value["name"],
								  & variablesOption	= value["levels"];

				valid = nameOption.type() == Json::stringValue && variablesOption.type() == Json::arrayValue;

				if (!valid)
					break;
			}
		}
	}

	return valid;
}

void RepeatedMeasuresFactorsListBase::termsChangedHandler()
{
	const vector<pair<string, vector<string> > > &factors = _factorsModel->getFactors();
	vector<Options *> allOptions;
	
	for (const auto &factor : factors)
	{
		OptionVariables* levelVariables = new OptionVariables(true);
		vector<string> levels;
		for (const string &level : factor.second)
			levels.push_back(level);
		levelVariables->setValue(levels);

		Options* options = new Options();
		options->add("name", new OptionEncodableString(factor.first));
		options->add("levels", levelVariables);

		allOptions.push_back(options);
	}
	
	_boundTo->setValue(allOptions);	
}
