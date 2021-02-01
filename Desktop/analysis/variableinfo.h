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
	enum InfoType { VariableType, Labels, VariableTypeName, VariableTypeIcon, VariableTypeDisabledIcon, VariableTypeInactiveIcon };
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
		if (_provider != nullptr)	return _provider->requestInfo(term, info);
		else						return QVariant();
	}

	QVariant requestLabel(const Term &term) const
	{
		return requestInfo(term, VariableInfo::Labels);
	}

private:
	VariableInfoProvider *_provider = nullptr;
};

#endif // VARIABLEINFO_H

