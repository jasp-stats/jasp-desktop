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
	Json::Value updatedValue = value; // If the value has no types, then we need to update it.

	for (Json::Value& factor : updatedValue)
	{
		Json::Value types = factor.isMember("types") ? factor["types"] : Json::arrayValue;
		int i = 0;

		Terms initTerms;
		for (const Json::Value& termsJson : factor[fq(_optionKey)])
		{
			std::vector<std::string> components;

			if (allowInteraction())
			{
				// For interaction, each term is an array of strings
				for (const Json::Value& elt : termsJson)
					if (elt.isString())
						components.push_back(elt.asString());
			}
			else
				// If not, each term is just a string
				components.push_back(termsJson.asString());

			Term term(components);
			columnType type = columnType::unknown;
			if (types.size() <= i)
			{
				if (components.size() == 1)
					type = model()->getVariableRealType(tq(components[0]));
				types.append(columnTypeToString(type));
			}
			else
				type = columnTypeFromString(types[i].asString());
			term.setType(type);
			initTerms.add(term);

			i++;
		}
		factor["types"] = types;
		factors.push_back(ListModelFactorsForm::Factor(tq(factor["name"].asString()), tq(factor["title"].asString()), initTerms));
	}

	BoundControlBase::bindTo(updatedValue);
	
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
		row["types"] = Json::Value(Json::arrayValue);

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
		Json::Value typesJson(Json::arrayValue);

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
			typesJson.append(columnTypeToString(term.type()));
		}
		factorJson[fq(_optionKey)] = termsJson;
		factorJson["types"] = typesJson;
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
