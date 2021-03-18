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

#include "factorlevellistbase.h"

using namespace std;

FactorLevelListBase::FactorLevelListBase(QQuickItem *parent)
	: JASPListControl(parent), BoundControlBase(this)
{
	_controlType = ControlType::FactorLevelList;
}

void FactorLevelListBase::setUpModel()
{
	_factorLevelsModel = new ListModelFactorLevels(this);
	JASPListControl::setUpModel();
}

void FactorLevelListBase::setUp()
{
	JASPListControl::setUp();

	connect(this, &FactorLevelListBase::itemChanged, _factorLevelsModel, &ListModelFactorLevels::itemChanged);
	connect(this, &FactorLevelListBase::itemRemoved, _factorLevelsModel, &ListModelFactorLevels::itemRemoved);
}

void FactorLevelListBase::bindTo(const Json::Value& value)
{
	BoundControlBase::bindTo(value);

	vector<pair<string, vector<string> > > factors;
	
	for (const Json::Value& row : value)
	{
		vector<string> factorLevels;
		for (const Json::Value& level : row["levels"])
			factorLevels.push_back(level.asString());
		
		factors.push_back(make_pair(row["name"].asString(), factorLevels));
	}
	
	_factorLevelsModel->initFactors(factors);
}

Json::Value FactorLevelListBase::createJson()
{
	Json::Value result(Json::arrayValue);

	for (int i = 0; i < minFactors(); i++)
	{
		Json::Value factor(Json::objectValue);
		factor["name"] = fq(getFactorName(i + 1));

		Json::Value levels(Json::arrayValue);
		for (int j = 0; j < minLevels(); j++)
			levels.append(fq(getLevelName(j + 1)));

		factor["levels"] = levels;
		result.append(factor);
	}

	return result;
}

bool FactorLevelListBase::isJsonValid(const Json::Value &value)
{
	bool valid = value.isArray();

	if (valid)
	{
		for (const Json::Value& row : value)
		{
			valid = row.isObject() && row["name"].isString() && row["levels"].isArray();
			if (!valid) break;
		}
	}

	return valid;
}

void FactorLevelListBase::termsChangedHandler()
{
	Json::Value boundValue(Json::arrayValue);
	const vector<pair<string, vector<string> > > &factors = _factorLevelsModel->getFactors();
	
	for (const auto &factor : factors)
	{
		Json::Value row(Json::objectValue);
		row["name"] = factor.first;

		Json::Value levels(Json::arrayValue);
		for (const string &level : factor.second)
			levels.append(level);

		row["levels"] = levels;
		boundValue.append(row);
	}
	
	setBoundValue(boundValue);
}
