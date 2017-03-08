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

#ifndef OPTION_H
#define OPTION_H

#include <string>

#include <boost/signals2.hpp>
#include <boost/bind.hpp>

#include "../lib_json/json.h"

class Options;

class Option
{
public:
	Option(bool transient = false);
	virtual ~Option();

	virtual void init(const Json::Value &) { };
	virtual Json::Value asJSON() const = 0;
	virtual void set(const Json::Value& value) = 0;
	virtual Option *clone() const = 0;

	boost::signals2::signal<void (Option *)> changed;

	void blockSignals(bool block);

	bool isTransient() const;

protected:
	void notifyChanged();
	bool _isTransient;

private:

	int _signalsBlocked;
	bool _shouldSignalOnceUnblocked;

};

#endif // OPTION_H
