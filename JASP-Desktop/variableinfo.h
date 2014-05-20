#ifndef VARIABLEINFO_H
#define VARIABLEINFO_H

#include <QVariant>
#include "term.h"

class VariableInfo
{
public:
	enum InfoType { VariableType };
};

class VariableInfoProvider
{
	friend class VariableInfoConsumer;

protected:
	virtual QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const = 0;
};

class VariableInfoConsumer
{
public:
	VariableInfoConsumer()
	{
		_provider = NULL;
	}

	void setInfoProvider(VariableInfoProvider *provider)
	{
		_provider = provider;
	}

	QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const
	{
		if (_provider != NULL)
			return _provider->requestInfo(term, info);
		else
			return QVariant();
	}

private:
	VariableInfoProvider *_provider;
};

#endif // VARIABLEINFO_H
