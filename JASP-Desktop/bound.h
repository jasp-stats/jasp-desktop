#ifndef BOUND_H
#define BOUND_H

#include "options/option.h"

#include <QString>

class Bound
{
public:

	virtual void bindTo(Option *option) = 0;
	virtual void unbind() { }

	bool isIllegal() const { return _message != ""; }
	QString illegalMessage() const { return _message; }

	boost::signals2::signal<void (Bound *)> illegalChanged;

protected:

	void setIllegal(QString message)
	{
		if (_message != message)
		{
			_message = message;
			illegalChanged(this);
		}
	}

	void setLegal()
	{
		setIllegal("");
	}

private:
	QString _message;
};

#endif // BOUND_H
