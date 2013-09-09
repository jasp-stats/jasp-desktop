#ifndef OPTIONNUMBER_H
#define OPTIONNUMBER_H

#include "optioni.h"

#include <climits>
#include <string>

class OptionNumber : public OptionI<double>
{
public:
	OptionNumber(std::string name, double value, double min = -999999, double max = 999999, int dp = 2);

	virtual Json::Value asJSON() const;// override;
	virtual void set(Json::Value& value);// override;

	double min();
	double max();
	int dp();

protected:
	double _min;
	double _max;
	int _dp;
};

#endif // OPTIONNUMBER_H
