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
#include "utilities/qutils.h"
#include "variableslistbase.h"
#include "log.h"

using namespace std;

FactorsFormBase::FactorsFormBase(QQuickItem *parent)
	: JASPListControl(parent), BoundControlBase(this)
{
	_controlType			= ControlType::FactorsForm;
	_useControlMouseArea	= false;
	_containsVariables		= true;
	_mayUseFormula			= false;
	_useTermsInRSyntax		= false;
}

void FactorsFormBase::setUpModel()
{
	_factorsModel = new ListModelFactorsForm(this);

	JASPListControl::setUpModel();

	_availableVariablesListName = property("availableVariablesListName").toString();
	QVariant availableListVariant = property("availableVariablesList");
	_availableVariablesListItem = qobject_cast<JASPListControl *>(availableListVariant.value<QObject *>());

	connect(this, &FactorsFormBase::initializedChanged, this, &FactorsFormBase::countVariablesChanged);
}

void FactorsFormBase::bindTo(const Json::Value& value)
{
	ListModelFactorsForm::FactorVec factors;

	for (const Json::Value& factor : value)
	{
		Terms initTerms;
		for (const Json::Value& termJson : factor[fq(_optionKey)])
		{
			std::vector<std::string> components;

			if (termJson.isArray())
			{
				// For interaction, each term is an array of strings
				for (const Json::Value& elt : termJson)
					if (elt.isString())
						components.push_back(elt.asString());
			}
			else if (termJson.isString())
				// If not, each term is just a string
				components.push_back(termJson.asString());

			if (components.size() > 0)
			{
				columnTypeVec types;
				for (const std::string& component : components)
					types.push_back(model()->getVariableRealType(tq(component)));
				Term term(components, types);
				initTerms.add(term);
			}
		}
		factors.push_back(ListModelFactorsForm::Factor(tq(factor["name"].asString()), tq(factor["title"].asString()), initTerms));
	}

	BoundControlBase::bindTo(value);
	
	_factorsModel->initFactors(factors);
}

Json::Value FactorsFormBase::createJson() const
{
	Json::Value result(Json::arrayValue);

	for (int i = 0; i < _initNumberFactors; i++)
	{
		Json::Value row(Json::objectValue);
		row["name"] = fq(baseName() + QString::number(i + startIndex()));
		row["title"] = fq(baseTitle() + " " + QString::number(i + startIndex()));
		row[fq(_optionKey)] = Json::Value(Json::arrayValue);

		result.append(row);
	}
		
	return result;
}

bool FactorsFormBase::isJsonValid(const Json::Value &value) const
{
	bool valid = value.isArray();
	if (valid)
	{
		for (const Json::Value& factor : value)
		{
			valid = factor.isObject() && factor["name"].isString() && factor["title"].isString() && factor[fq(_optionKey)].isArray();
			if (!valid) break;
		}
	}

	return valid;
}

void FactorsFormBase::termsChangedHandler()
{
	// This slot is called to set the boundValues if the factors are changed.
	// This does not have to be called during the initialization of the FactorsForm: the _factorsModel might give the wrong information
	// and during the initialization, the boundValues has to be set by the bindTo method anyway.
	if (!initialized()) return;

	JASPListControl::termsChangedHandler();

	const ListModelFactorsForm::FactorVec &factors = _factorsModel->getFactors();
	Json::Value boundValue(Json::arrayValue);
	
	for (const auto &factor : factors)
	{
		Json::Value factorJson(Json::objectValue);
		factorJson["name"] = fq(factor.name);
		factorJson["title"] = fq(factor.title);
		Json::Value termsJson(Json::arrayValue);

		for (const Term &term : factor.listView ? factor.listView->model()->terms() : factor.initTerms)
		{
			Json::Value termJson(allowInteraction() ? Json::arrayValue : Json::stringValue);
			if (allowInteraction())
			{
				for (const std::string & elt : term.scomponents())
					termJson.append(elt);
			}
			else
				termJson = term.asString();
			termsJson.append(termJson);
		}
		factorJson[fq(_optionKey)] = termsJson;
		boundValue.append(factorJson);
	}
	
	setBoundValue(boundValue);
}

void FactorsFormBase::factorAdded(int index, QVariant item)
{
	VariablesListBase* listView = item.value<VariablesListBase *>();
	if (!listView)
	{
		JASPControl* control = item.value<JASPControl *>();
		Log::log() << "JASP Control " << (control ? control->name() : "") << " is not a VariablesListBase in factorAdded" << std::endl;
		return;
	}
	
	_factorsModel->factorAdded(index, listView);
	
	connect(listView->model(), &ListModel::termsChanged, _factorsModel, &ListModelFactorsForm::resetModelTerms, Qt::QueuedConnection);
	connect(listView->model(), &ListModel::termsChanged, this, &FactorsFormBase::countVariablesChanged);
	connect(listView->model(), &ListModel::termsChanged, _factorsModel, &ListModelFactorsForm::ensureNesting);
}
