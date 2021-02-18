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
#include "analysis/analysisform.h"
#include "utilities/qutils.h"
#include "variableslistbase.h"
#include "log.h"

using namespace std;

FactorsFormBase::FactorsFormBase(QQuickItem *parent)
	: JASPListControl(parent), BoundControlBase(this)
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
}

void FactorsFormBase::bindTo(const Json::Value& value)
{
	BoundControlBase::bindTo(value);

	ListModelFactorsForm::FactorVec factors;

	for (const Json::Value& factor : value)
	{
		vector<string> indicators;
		for (const Json::Value& indicator : factor["indicators"])
			indicators.push_back(indicator.asString());
		
		factors.push_back(make_tuple(factor["name"].asString(), factor["title"].asString(), indicators));
	}
	
	_factorsModel->initFactors(factors);
}

Json::Value FactorsFormBase::createJson()
{
	Json::Value result(Json::arrayValue);

	for (int i = 0; i < _initNumberFactors; i++)
	{
		Json::Value row(Json::objectValue);
		QString name("Factor");
		name += QString::number(i+1);
		QString title("Factor ");
		title += QString::number(i+1);
		row["name"] = fq(name);
		row["title"] = fq(title);
		row["indicators"] = Json::Value(Json::arrayValue);

		result.append(row);
	}
		
	return result;
}

bool FactorsFormBase::isJsonValid(const Json::Value &value)
{
	bool valid = value.isArray();
	if (valid)
	{
		for (const Json::Value& factor : value)
		{
			valid = factor.isArray() && factor["name"].isString() && factor["title"].isString() && factor["indicators"].isArray();
			if (!valid) break;
		}
	}

	return valid;
}

void FactorsFormBase::termsChangedHandler()
{
	const ListModelFactorsForm::FactorVec &factors = _factorsModel->getFactors();
	Json::Value boundValue(Json::arrayValue);
	
	for (const auto &factor : factors)
	{
		Json::Value factorJson(Json::objectValue);
		factorJson["name"] = get<0>(factor);
		factorJson["title"] = get<1>(factor);
		Json::Value indicators(Json::arrayValue);
		for (const string &level : get<2>(factor))
			indicators.append(level);
		factorJson["indicators"] = indicators;
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
	
	connect(listView->model(), &ListModel::termsChanged, _factorsModel, &ListModelFactorsForm::resetModelTerms);
}
