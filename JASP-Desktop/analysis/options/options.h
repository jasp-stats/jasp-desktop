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

#ifndef OPTIONS_H
#define OPTIONS_H

#include "option.h"
#include "common.h"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range.hpp>

#include "jsonredirect.h"

typedef std::pair<std::string, Option*> OptionNamed;

class Options : public Option
{

public:
	Options() : Option(), names(&_options) {}
	~Options();

	Option*		clone()									const	override;
	void		set(const Json::Value &json)					override;
	Json::Value asJSON()								const	override;
	Json::Value asMetaJSON()							const	override;
	Json::Value asJSON(bool includeTransient)			const;
	Json::Value asJSONWithMeta()						const;

	void		add(std::string name, Option *option);
	void		remove(std::string name);
	void		clear()	override;
	size_t		size()									const				{ return _options.size(); }

	Option*		get(std::string name)					const;
	Option*		get(int index)							const				{ return _options.at(size_t(index)).second; }
	void		get(int index, std::string &name, Option *&option);	

	std::set<std::string>	usedVariables()																	const	override;
	void					removeUsedVariable(const std::string & var)												override;
	void					replaceVariableName(const std::string & oldName, const std::string & newName)			override;
	std::set<std::string>	columnsCreated()																		override;
	void					replaceKey(const std::string& oldKey, const std::string& newKey);

	class Names
	{
		friend class Options;
	public:

		class iterator : public boost::iterator_facade<iterator, const std::string, boost::forward_traversal_tag>
		{
			friend class boost::iterator_core_access;
			friend class Names;

		private:
			explicit	iterator(std::vector<OptionNamed>::const_iterator parent) { _parent = parent; }

						void			increment()								{ _parent++; }
						bool			equal(iterator const& other)	const	{ return _parent == other._parent;	}
			const		std::string&	dereference()					const	{ return (*_parent).first;			}

			std::vector<OptionNamed>::const_iterator _parent;
		};

		iterator begin()	const { return iterator(_options->begin()); }
		iterator end()		const { return iterator(_options->end());	}

	private:
		std::vector<OptionNamed> *_options;
		Names(std::vector<OptionNamed> *options) { _options = options; }
	};

	Names names;

private:

	std::vector<OptionNamed> _options;

	static void insertValue(const std::string &name, Json::Value& value, Json::Value &root);
	static bool extractValue(const std::string &name, const Json::Value &root, Json::Value &value);

	void optionsChanged(Option *option);


};

namespace boost
{
	template <>
	struct range_const_iterator< Options::Names >
	{
		typedef Options::Names::iterator type;
	};
}

#endif // OPTIONS_H
