//
// Copyright (C) 2013-2020 University of Amsterdam
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

#ifndef BOUNDCONTROL_H
#define BOUNDCONTROL_H

#include "jsonredirect.h"

class BoundControl
{

public:
	virtual Json::Value					createJson()													= 0;
	virtual Json::Value					createMeta()													= 0;
	virtual bool						isJsonValid(const Json::Value& optionValue)						= 0;
	virtual void						bindTo(const Json::Value& value)								= 0;
	virtual const Json::Value&			boundValue()													= 0;
	virtual void						resetBoundValue()												= 0;
	virtual std::vector<std::string>	usedVariables()													= 0;
	virtual void						setBoundValue(const Json::Value& value, bool emitChange = true) = 0;
};

#endif // BOUNDCONTROL_H
