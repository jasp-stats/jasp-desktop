//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include "qquick/jasptheme.h"

class VariableInfo
{
public:
	enum InfoType { VariableType, Labels, VariableTypeName };
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
	VariableInfoConsumer() {}

	void setInfoProvider(VariableInfoProvider *provider)
	{
		_provider = provider;
	}

	QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const
	{
		if (_provider != NULL)	return _provider->requestInfo(term, info);
		else					return QVariant();
	}

	bool variableIsColumn(const Term &term)
	{
		switch (requestInfo(term, VariableInfo::VariableType).toInt())
		{
		case int(columnType::nominalText):
		case int(columnType::nominal):
		case int(columnType::ordinal):
		case int(columnType::scale):		return true;
		default:									return false;
		}
	}

	QVariant requestIcon(const Term &term) const
	{
		static QString	nominalTextIcon	= "variable-nominal-text.svg",
						nominalIcon		= "variable-nominal.svg",
						ordinalIcon		= "variable-ordinal.svg",
						scaleIcon		= "variable-scale.svg";

		switch (requestInfo(term, VariableInfo::VariableType).toInt())
		{
		case int(columnType::nominalText):	return JaspTheme::currentIconPath() + nominalTextIcon;
		case int(columnType::nominal):		return JaspTheme::currentIconPath() + nominalIcon;
		case int(columnType::ordinal):		return JaspTheme::currentIconPath() + ordinalIcon;
		case int(columnType::scale):		return JaspTheme::currentIconPath() + scaleIcon;
		default:							return QVariant();
		}
	}

	QVariant requestLabel(const Term &term) const
	{
		return requestInfo(term, VariableInfo::Labels);
	}

private:
	VariableInfoProvider *_provider = NULL;
};

#endif // VARIABLEINFO_H
