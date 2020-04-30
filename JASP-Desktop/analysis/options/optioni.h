//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef OPTIONI_H
#define OPTIONI_H

#include "option.h"
#include "common.h"

template <class T>
class OptionI : public Option
{

public:
	OptionI(bool transient = false)				: Option(transient)					{ }
	OptionI(T value, bool transient = false)	: Option(transient), _value(value)	{ }

	virtual T		value()	const				{ return _value; }
	virtual void	setValue(const T &value)
	{
		if (_value == value)
			return;

		_value = value;

		notifyChanged(this);
	}

protected:
	T _value;

};

#endif // OPTIONI_H
