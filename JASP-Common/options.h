#ifndef OPTIONS_H
#define OPTIONS_H

#include "option.h"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range.hpp>

#include "lib_json/json.h"

class Options
{

public:
    Options();

	void add(Option* option);
	Json::Value asJSON() const;

    Option *get(std::string name) const;

    boost::signals2::signal<void ()> onChange;

	typedef std::vector<Option*>::iterator iterator;

	iterator begin();
	iterator end();

private:

	static void insertValue(std::string &name, Json::Value& value, Json::Value &root);

	std::map<std::string, Option*> _store;
	std::vector<Option*> _options;

	void optionsChanged();

};

namespace boost
{
	// specialize range_mutable_iterator and range_const_iterator in namespace boost
	/*template<>
	struct range_mutable_iterator< Column::AsInt >
	{
		typedef Column::AsInt::iterator type;
	};*/

	template <>
	struct range_const_iterator< Options >
	{
		typedef Options::iterator type;
	};
}

#endif // OPTIONS_H
