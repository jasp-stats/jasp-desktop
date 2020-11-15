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

#ifndef BOUNDCONTROL_H
#define BOUNDCONTROL_H

#include "option.h"

#include <QString>

class BoundControl
{
public:
	virtual ~BoundControl() {}
	virtual void bindTo(Option *option)			= 0;
	virtual void unbind()						{ }
	virtual Option* createOption()				= 0;
	virtual Option* boundTo()					= 0;
	virtual bool isOptionValid(Option* option)	= 0;
	virtual bool isJsonValid(const Json::Value& optionValue) = 0;
	virtual void modelChanged()					{ }
};

#endif // BOUNDCONTROL_H
