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
class ComputedColumn;


template <typename T> struct return_not_NULL
{
	typedef T result_type;

	template <typename InputIterator> T operator()(InputIterator first, InputIterator last) const
	{
		T result = NULL;
		for(auto aResult = first; aResult != last; aResult++)
			if(*aResult != NULL)
				result = *aResult;

		return result;
	}
};

class Option
{
public:
						Option(bool transient = false) : _isTransient(transient) {}

	virtual				~Option() {}
	virtual void		init(const Json::Value &)		{ }
	virtual void		set(const Json::Value& value)	= 0;
	virtual Json::Value asJSON()		const			= 0;
	virtual Json::Value	asMetaJSON()	const			{ return Json::nullValue; }
	virtual Option		*clone()		const			= 0;
	virtual void		clear() {}


	virtual std::set<std::string>	usedVariables()		const														{ return std::set<std::string>(); }
	virtual void					removeUsedVariable(const std::string &)											{}
	virtual void					replaceVariableName(const std::string & oldName, const std::string & newName)	{}
	virtual std::set<std::string>	columnsCreated()																{ return std::set<std::string>(); }

			void		blockSignals(bool block, bool notifyOnceUnblocked = true);
			bool		isTransient() const;


	boost::signals2::signal<void				(Option *)>											changed;
	boost::signals2::signal<void				(std::string, int)>									requestColumnCreation;
	boost::signals2::signal<void				(std::string)>										requestComputedColumnDestruction;
	boost::signals2::signal<ComputedColumn *	(std::string), return_not_NULL<ComputedColumn *>>	requestComputedColumnCreation;

	void				notifyRequestColumnCreation(std::string columnName, int columnType)	{ return requestColumnCreation(columnName, columnType); }
	ComputedColumn *	notifyRequestComputedColumnCreation(std::string columnName)			{ return requestComputedColumnCreation(columnName); }
	void				notifyRequestComputedColumnDestruction(std::string columnName)		{ requestComputedColumnDestruction(columnName); }
	
	Json::Value			defaultMetaEntryContainingColumn(bool containsColumn = true) const;

protected:
	void				notifyChanged();

	bool		_isTransient;

private:
	int			_signalsBlocked						= 0;
	bool		_shouldSignalChangedOnceUnblocked	= false;

};

#endif // OPTION_H
