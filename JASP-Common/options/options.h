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
	virtual void set(Json::Value &json) OVERRIDE;

	void add(std::string name, Option *option);
    Option *get(std::string name) const;
	Option *get(int index);
	void get(int index, std::string &name, Option *&option);

	virtual Option* clone() const OVERRIDE;

	size_t size();

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

	static void insertValue(std::string &name, Json::Value& value, Json::Value &root);
	static Json::Value extractValue(std::string &name, Json::Value &root);

	void optionsChanged();

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
