//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

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
