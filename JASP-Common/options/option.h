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
	Option();
	virtual ~Option();

	virtual Json::Value asJSON() const = 0;
	virtual void set(Json::Value& value) = 0;
	virtual Option *clone() const = 0;

	boost::signals2::signal<void (Option *)> changed;

	void blockSignals(bool block);

protected:
	void notifyChanged();

private:
	int _signalsBlocked;
	bool _shouldSignalOnceUnblocked;

};

#endif // OPTION_H
