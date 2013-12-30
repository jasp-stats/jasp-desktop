#include "optionnumber.h"

using namespace std;

OptionNumber::OptionNumber(double value, double min, double max, string format)
{
	_value = value;
	_min = min;
	_max = max;
	_format = format;
}

Json::Value OptionNumber::asJSON() const
{
	return Json::Value(_value);
}

void OptionNumber::set(Json::Value &value)
{
	_value = value.asDouble();
}

void OptionNumber::setValue(double value)
{
	if (_format == "%")
		value /= 100.0;

	OptionI::setValue(value);
}

double OptionNumber::value() const
{
	double value = OptionI::value();

	if (_format == "%")
		value *= 100.0;

	return value;
}

Option *OptionNumber::clone() const
{
	return new OptionNumber(_value, _min, _max, _format);
}

double OptionNumber::min()
{
	return _min;
}

double OptionNumber::max()
{
	return _max;
}

string OptionNumber::format()
{
	return _format;
}
