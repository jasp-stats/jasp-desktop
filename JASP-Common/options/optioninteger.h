//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef OPTIONINTEGER_H
#define OPTIONINTEGER_H

#include "optioni.h"
#include "common.h"

#include <climits>
#include <string>

class OptionInteger : public OptionI<int>
{
public:
	OptionInteger(int value = 0, int min = -999999, int max = 999999, std::string format = "");

	virtual void init(const Json::Value &data) OVERRIDE;
	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

  int min() const;
  int max() const;

  std::string format() const;

protected:
  int _min;
  int _max;
  std::string _format;
};

#endif // OPTIONINTEGER_H
