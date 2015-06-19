#ifndef OPTIONNUMBER_H
#define OPTIONNUMBER_H

#include "optioni.h"
#include "common.h"

#include <climits>
#include <string>

class OptionNumber : public OptionI<double>
{
public:
	OptionNumber();
	OptionNumber(double value, double min = -999999, double max = 999999, std::string format = "");

	virtual void init(const Json::Value &data) OVERRIDE;

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	double min() const;
	double max() const;

	std::string format() const;

protected:
	double _min;
	double _max;
	std::string _format;
};

#endif // OPTIONNUMBER_H
