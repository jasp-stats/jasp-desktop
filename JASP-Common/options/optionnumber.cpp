#include "optionnumber.h"

using namespace std;

OptionNumber::OptionNumber(string name, double value, double min, double max, int dp)
	: OptionI(name)
{
	_value = value;
	_min = min;
	_max = max;
	_dp = dp;
}

Json::Value OptionNumber::asJSON() const
{
	return Json::Value(_value);
}

void OptionNumber::set(Json::Value &value)
{
	_value = value.asDouble();
}

double OptionNumber::min()
{
	return _min;
}

double OptionNumber::max()
{
	return _max;
}

int OptionNumber::dp()
{
	return _dp;
}
