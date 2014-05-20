#ifndef OPTIONI_H
#define OPTIONI_H

#include "option.h"
#include "common.h"

#include <QDebug>

template <class T>
class OptionI : public Option
{

public:

	OptionI() : Option()
	{
	}

	virtual T value() const
	{
		return _value;
	}

	virtual void setValue(T value)
	{
		if (_value == value)
			return;

		_value = value;

		notifyChanged();
	}

protected:
	T _value;

};

#endif // OPTIONI_H
