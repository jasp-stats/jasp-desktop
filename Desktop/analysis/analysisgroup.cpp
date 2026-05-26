//
// Copyright (C) 2013-2025 University of Amsterdam
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

#include "analysisgroup.h"

AnalysisGroup::AnalysisGroup(size_t id, const std::string & title)
	: _id(id), _title(title)
{}

Json::Value AnalysisGroup::asJson() const
{
	Json::Value j	= Json::objectValue;
	j["id"]			= int(_id);
	j["title"]		= _title;
	return j;
}

AnalysisGroup * AnalysisGroup::fromJson(const Json::Value & json)
{
	size_t		id		= size_t(json.get("id", 0).asInt());
	std::string title	= json.get("title", "Group").asString();
	return new AnalysisGroup(id, title);
}
