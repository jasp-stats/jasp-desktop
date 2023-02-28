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
#include <QAbstractItemModel>
#include "columntype.h"

class VariableInfoProvider;

// The Provider/Consumer mechanism makes an interface so that the consumers (the QML models) get their data without having to know how the Provider furnishes this data
// Typically, for a JASP application, the Provider will be the ColumnsModel, but if the QML forms are used somewhere else, another Provider should be instantiated.
// We need a QObject class so that we can use the Qt signals.
// The VariableInfoProvider and VariableInfoConsumer are classes used by ColumnsModel, ListModel or SourceItem, that are already QObject classes.
// As a class cannot derive from 2 QObject classes, the VariableInfoProvider and VariableInfoConsumer cannot be QObject classes.
// So we just use the VariableInfo class (which is a singleton intialized at the start of the application) to propagate the signals
class VariableInfo : public QObject
{
	Q_OBJECT
public:
	enum InfoType { VariableType, VariableTypeName, VariableTypeIcon, VariableTypeDisabledIcon, VariableTypeInactiveIcon, VariableNames, RowCount, Labels, StringValues, DoubleValues, NameRole, Value, MaxWidth, SignalsBlocked };
	enum IconType { DefaultIconType, DisabledIconType, InactiveIconType };

public:
	VariableInfo(VariableInfoProvider* provider);

	static VariableInfo*		info();
	static QString				getIconFile(columnType colType, IconType type);

	VariableInfoProvider*		provider()	{ return _provider; }

signals:
	void namesChanged(QMap<QString, QString> changedNames);
	void columnsChanged(QStringList changedColumns);
	void columnTypeChanged(QString colName);
	void labelsChanged(QString columnName, QMap<QString, QString> changedLabels);
	void labelsReordered(QString columnName);

private:
	VariableInfoProvider *	_provider	= nullptr;

	static VariableInfo *_singleton;
};

class VariableInfoProvider
{
public:
	virtual QVariant				provideInfo(VariableInfo::InfoType info, const QString& name = "", int row = 0)	const	= 0;
	virtual QAbstractItemModel*		providerModel()																			{ return nullptr;			}
};

class VariableInfoConsumer
{
public:
	VariableInfoConsumer() { _provider = (VariableInfo::info() ? VariableInfo::info()->provider() : nullptr);
}

	QVariant				requestInfo(VariableInfo::InfoType info, const QString &name = "", int row = 0)	const	{ return _provider ? _provider->provideInfo(info, name, row)	: QVariant();	}
	bool					isInfoProviderModel(QObject* model)												const	{ return _provider ? model == _provider->providerModel()		: false;		}
	QAbstractItemModel*		infoProviderModel()																		{ return _provider ? _provider->providerModel()					: nullptr;		}

private:
	VariableInfoProvider *_provider = nullptr;
};

#endif // VARIABLEINFO_H

