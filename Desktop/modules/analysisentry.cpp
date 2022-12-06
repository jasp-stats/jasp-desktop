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


#include "dynamicmodule.h"
#include "jasptheme.h"

namespace Modules
{

DynamicModule*	AnalysisEntry::dynamicModule() const
{
	return _dynamicModule;
}

std::string AnalysisEntry::qmlFilePath() const
{
	return dynamicModule() ? dynamicModule()->qmlFilePath(_qml) : "";
}

std::string AnalysisEntry::icon() const
{
	if(_icon == "")
		return _isGroupTitle ? fq(JaspTheme::currentIconPath()) + "large-arrow-right.png" : "";

	return _dynamicModule  ? "file:" + _dynamicModule->iconFilePath(_icon) : "qrc:/icons/" + _icon;
}

std::string AnalysisEntry::getFullRCall() const
{
	return dynamicModule()->rModuleCall(functionInternal());
}

Json::Value AnalysisEntry::getDefaultResults() const
{
	Json::Value res(Json::objectValue),
				metaEnt(Json::objectValue);

	res["title"]			= title();
	res[".meta"]			= Json::arrayValue;
	res["notice"]			= Json::objectValue;
	res["notice"]["title"]	= "Waiting for initialization of module: " + dynamicModule()->title();
	res["notice"]["height"] = 0;
	res["notice"]["width"]	= 0;

	metaEnt["name"]			= "notice";
	metaEnt["type"]			= "image"; //pretending it is a plot to make it show up at least, width == height == 0 to make sure no space is wasted
	res[".meta"].append(metaEnt);

	return res;
}

Json::Value AnalysisEntry::asJsonForJaspFile()	const
{
	return dynamicModule()->asJsonForJaspFile(function());
}

std::string AnalysisEntry::codedReference() const
{
	return dynamicModule()->name() + "~" + function();
}

std::string	AnalysisEntry::buttonMenuString() const
{
	return dynamicModule() == nullptr ? function() : codedReference();
}

} // namespace Modules
