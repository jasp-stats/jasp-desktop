#ifndef VARIABLEINFO_H
#define VARIABLEINFO_H

#include <QVariant>
#include <QIcon>
#include "term.h"
#include "column.h"

class VariableInfo
{
public:
	enum InfoType { VariableType, Labels };
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

	QVariant requestIcon(const Term &term) const
	{
		static QIcon nominalTextIcon = QIcon(":/icons/variable-nominal-text.svg");
		static QIcon nominalIcon = QIcon(":/icons/variable-nominal.svg");
		static QIcon ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
		static QIcon scaleIcon = QIcon(":/icons/variable-scale.svg");

		int type = requestInfo(term, VariableInfo::VariableType).toInt();

		switch (type)
		{
		case Column::ColumnTypeNominalText:
			return nominalTextIcon;
		case Column::ColumnTypeNominal:
			return nominalIcon;
		case Column::ColumnTypeOrdinal:
			return ordinalIcon;
		case Column::ColumnTypeScale:
			return scaleIcon;
		default:
			return QVariant();
		}
	}

private:
	VariableInfoProvider *_provider;
};

#endif // VARIABLEINFO_H
