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
