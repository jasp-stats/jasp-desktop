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

#ifndef OPTIONS_H
#define OPTIONS_H

#include "option.h"
#include "../common.h"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range.hpp>

#include "../lib_json/json.h"

typedef std::pair<std::string, Option*> OptionNamed;

class Options : public Option
{

public:
	Options();
	~Options();

	virtual Json::Value asJSON() const OVERRIDE;
	Json::Value asJSON(bool includeTransient) const;
	void init(const Json::Value &data) OVERRIDE;
	virtual void set(const Json::Value &json) OVERRIDE;
	Option* createOption(std::string typeString);
	void add(std::string name, Option *option);
	Option *get(std::string name) const;
	Option *get(int index);
	void get(int index, std::string &name, Option *&option);

	virtual Option* clone() const OVERRIDE;

	size_t size() const;

	class Names
	{
		friend class Options;

	private:

		std::vector<OptionNamed> *_options;



		Names(std::vector<OptionNamed> *options)
		{
			_options = options;
		}

	public:

		class iterator : public boost::iterator_facade<
				iterator, const std::string, boost::forward_traversal_tag>
		{
			friend class boost::iterator_core_access;
			friend class Names;

		private:

			explicit iterator(std::vector<OptionNamed>::const_iterator parent)
			{
				_parent = parent;
			}

			std::vector<OptionNamed>::const_iterator _parent;

			void increment()
			{
				_parent++;
			}

			bool equal(iterator const& other) const
			{
				return _parent == other._parent;
			}

			const std::string& dereference() const
			{
				return (*_parent).first;
			}

		};

		iterator begin() const
		{
			return iterator(_options->begin());
		}

		iterator end() const
		{
			return iterator(_options->end());
		}
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
