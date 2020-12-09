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

#include "factorsformbase.h"
#include "boundcontrolterms.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "analysis/analysisform.h"
#include "analysis/jaspcontrol.h"
#include "utilities/qutils.h"

#include <QQmlProperty>
#include <QQuickItem>

using namespace std;

FactorsFormBase::FactorsFormBase(QQuickItem *parent)
	: JASPListControl(parent)
{
	_controlType			= ControlType::FactorsForm;
	_useControlMouseArea	= false;
}

void FactorsFormBase::setUpModel()
{
	_factorsModel = new ListModelFactorsForm(this);
	JASPListControl::setUpModel();

	_availableVariablesListName = property("availableVariablesListName").toString();
	QVariant availableListVariant = property("availableVariablesList");
	_availableVariablesListItem = dynamic_cast<JASPControl*>(qobject_cast<QQuickItem *>(availableListVariant.value<QObject *>()));
	_initNumberFactors = property("initNumberFactors").toInt();

	QQuickItem::connect(this, SIGNAL(titleChanged(int, QString)), _factorsModel, SLOT(titleChangedSlot(int, QString)));
	QQuickItem::connect(this, SIGNAL(factorAdded(int, QVariant)), _factorsModel, SLOT(factorAddedSlot(int, QVariant)));

	connect(_factorsModel, &ListModelFactorsForm::addListView, this, &FactorsFormBase::addListViewSlot);
}

void FactorsFormBase::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	
	vector<tuple<string, string, vector<string> > > factors;
	vector<Options*> allOptions = _boundTo->value();
	
	for (const Options* options : allOptions)
	{
		OptionString *factorNameOption = static_cast<OptionString *>(options->get("name"));
		OptionString *factorTitleOption = static_cast<OptionString *>(options->get("title"));
		OptionVariables *factorLevelsOption = static_cast<OptionVariables *>(options->get("indicators"));
		
		factors.push_back(make_tuple(factorNameOption->value(), factorTitleOption->value(), factorLevelsOption->variables()));
	}
	
	_factorsModel->initFactors(factors);
}

Option* FactorsFormBase::createOption()
{
	
	Options* templote = new Options();
	templote->add("name", new OptionString());
	templote->add("title", new OptionString());
	templote->add("indicators", new OptionVariables());
	
	OptionsTable* optionsTable = new OptionsTable(templote);
	std::vector<Options*> allOptions;
	
	for (int i = 0; i < _initNumberFactors; i++)
	{
		Options* options = new Options();
		QString name("Factor");
		name += QString::number(i+1);
		QString title("Factor ");
		title += QString::number(i+1);
		options->add("name", new OptionString(name.toStdString()));
		options->add("title", new OptionString(title.toStdString()));
		OptionVariables* levels = new OptionVariables();
		options->add("indicators", levels);
		allOptions.push_back(options);
	}
	
	optionsTable->connectOptions(allOptions);
	
	return optionsTable;
}

bool FactorsFormBase::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool FactorsFormBase::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::arrayValue;
}

void FactorsFormBase::termsChangedHandler()
{
	const vector<tuple<string, string, vector<string> > > &factors = _factorsModel->getFactors();
	vector<Options *> allOptions;
	
	for (const auto &factor : factors)
	{
		Options* options = new Options();
		options->add("name", new OptionString(get<0>(factor)));
		options->add("title", new OptionString(get<1>(factor)));
		OptionVariables* levelVariables = new OptionVariables();
		vector<string> levels;
		for (const string &level : get<2>(factor))
			levels.push_back(level);
		levelVariables->setValue(levels);
		options->add("indicators", levelVariables);
		allOptions.push_back(options);
	}
	
	_boundTo->setValue(allOptions);	
}

void FactorsFormBase::addListViewSlot(JASPListControl *listView)
{
	const vector<tuple<string, string, vector<string> > > &factors = _factorsModel->getFactors();
	QStringList names;
	for (auto factor : factors)
		names.push_back(tq(get<0>(factor)));
	_availableVariablesListItem->setProperty("dropKeys", names);
	setProperty("dropKeys", _availableVariablesListName);
	
	JASPListControl* availableListView = dynamic_cast<JASPListControl*>(_availableVariablesListItem);
	form()->addListView(listView, availableListView);
	
	connect(listView->model(), &ListModel::modelReset, this, &FactorsFormBase::termsChangedHandler);
	connect(listView->model(), &ListModel::rowsRemoved, this, &FactorsFormBase::termsChangedHandler);
	connect(listView->model(), &ListModel::rowsInserted, this, &FactorsFormBase::termsChangedHandler);
}
