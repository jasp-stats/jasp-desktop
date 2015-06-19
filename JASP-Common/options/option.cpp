#include "option.h"

using namespace std;

Option::Option(bool transient)
{
	_isTransient = transient;
	_signalsBlocked = 0;
	_shouldSignalOnceUnblocked = false;
}

Option::~Option()
{
}

void Option::blockSignals(bool block)
{
	if (block)
	{
		_signalsBlocked++;
	}
	else
	{
		_signalsBlocked--;
		if (_signalsBlocked < 0)
			_signalsBlocked = 0;

		if (_signalsBlocked == 0 && _shouldSignalOnceUnblocked)
		{
			changed(this);
			_shouldSignalOnceUnblocked = false;
		}
	}
}

bool Option::isTransient() const
{
	return _isTransient;
}

void Option::notifyChanged()
{
	if (_signalsBlocked)
		_shouldSignalOnceUnblocked = true;
	else
		changed(this);
}





