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

using namespace std;

RepeatedMeasuresFactorsListBase::RepeatedMeasuresFactorsListBase(QQuickItem *parent)
	: JASPListControl(parent), BoundControlBase(this)
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

	connect(this, &RepeatedMeasuresFactorsListBase::itemChanged, _factorsModel, &ListModelRepeatedMeasuresFactors::itemChanged);
	connect(this, &RepeatedMeasuresFactorsListBase::itemRemoved, _factorsModel, &ListModelRepeatedMeasuresFactors::itemRemoved);
}

void RepeatedMeasuresFactorsListBase::bindTo(const Json::Value& value)
{
	vector<pair<string, vector<string> > > factors;
	
	for (const Json::Value& row : value)
	{
		vector<string> factorLevels;
		for (const Json::Value& level : row["levels"])
			factorLevels.push_back(level.asString());
		
		factors.push_back(make_pair(row["name"].asString(), factorLevels));
	}
	
	_factorsModel->initFactors(factors);

	BoundControlBase::bindTo(value);
}

Json::Value RepeatedMeasuresFactorsListBase::createJson()
{
	Json::Value result(Json::arrayValue);

	for (const QString& factorStr : defaultFactors())
	{
		Json::Value factor(Json::objectValue);
		factor["name"] = fq(factorStr);

		Json::Value levels(Json::arrayValue);
		for (const QString& levelStr: defaultLevels())
			levels.append(fq(levelStr));

		factor["levels"] = levels;
		result.append(factor);
	}

	return result;
}

bool RepeatedMeasuresFactorsListBase::isJsonValid(const Json::Value &value)
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

void RepeatedMeasuresFactorsListBase::termsChangedHandler()
{
	Json::Value boundValue(Json::arrayValue);
	const vector<pair<string, vector<string> > > &factors = _factorsModel->getFactors();
	
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
