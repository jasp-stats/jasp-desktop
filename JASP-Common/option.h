#ifndef OPTION_H
#define OPTION_H

#include <string>

#include <boost/signals2.hpp>
#include <boost/bind.hpp>

#include "lib_json/json.h"

class Option
{
public:
    Option(std::string name);
    std::string name();

	virtual Json::Value asJSON() const = 0;

	boost::signals2::signal<void (Option *)> changed;

private:

    std::string _name;
};

#endif // OPTION_H
