#ifndef OPTIONI_H
#define OPTIONI_H

#include "option.h"

#include <QDebug>

template <class T>
class OptionI : public Option
{

public:

    OptionI(std::string name) : Option(name)
	{

	}

	T value() const
	{
		return _value;
	}

	virtual void setValue(T value)
	{
		_value = value;

		changed(this);
	}

protected:
	T _value;

};

#endif // OPTIONI_H
