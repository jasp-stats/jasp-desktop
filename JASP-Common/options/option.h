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

#ifndef OPTION_H
#define OPTION_H

#include <string>
#include <set>

#include <boost/signals2.hpp>
#include <boost/bind.hpp>
#include "jsonredirect.h"

class Options;
class Analysis;

class Option
{
public:
						Option(bool transient = false) : _isTransient(transient) {}

	virtual				~Option() {}
	virtual void		init(const Json::Value &)		{ }
	virtual void		set(const Json::Value& value)	= 0;
	virtual Json::Value asJSON() const					= 0;
	virtual Option		*clone() const					= 0;

	virtual std::set<std::string> usedVariables()		{ return std::set<std::string>(); }
	virtual void		removeUsedVariable(std::string)	{  }

			void		blockSignals(bool block, bool notifyOnceUnblocked = true);
			bool		isTransient() const;


	boost::signals2::signal<void (Option *)> changed;


	
protected:
	void	notifyChanged();
	bool	_isTransient;

private:
	int		_signalsBlocked				= 0;
	bool	_shouldSignalOnceUnblocked	= false;

};

#endif // OPTION_H
