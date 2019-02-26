#include "jaspdoublevalidator.h"

QValidator::State JASPDoubleValidator::validate(QString & s, int & pos) const
{
	if (s.isEmpty() || (s.startsWith("-") && s.length() == 1 && bottom() < 0))
	{
		// allow empty field or standalone minus sign
		return QValidator::Intermediate;
	}
	// check length of decimal places
	QChar point = locale().decimalPoint();
	if (s.indexOf(point) != -1)
	{
		if (decimals() == 0)
			return QValidator::Invalid;
		int lengthDecimals = s.length() - s.indexOf(point) - 1;
		if (lengthDecimals > decimals())
			return QValidator::Invalid;
	}
	// check range of value
	bool isNumber;
	double value = locale().toDouble(s, &isNumber);
	if (isNumber && bottom() <= value && value <= top())
		return QValidator::Acceptable;
	
	return QValidator::Invalid;
}

