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

#ifndef ROWCOMPONENTS_H
#define ROWCOMPONENTS_H

#include "common.h"
#include <QQmlComponent>
#include <QObject>

class ListModel;
class JASPControlWrapper;
class JASPControlBase;
class Option;

class RowControls : public QObject
{
Q_OBJECT

public:
	
	RowControls(ListModel* parent, QVector<QQmlComponent *>& components, const QMap<QString, Option*>& rowOptions);
	virtual ~RowControls() {}
	
	void										setContext(int row, const QString& key);
	const QList<QVariant>&						getControls() const		{ return _rowControls; }
	const QMap<QString, JASPControlWrapper*>&	getControlsMap() const	{ return _rowControlsMap; }

private:
	ListModel*							_parentModel;
	QMap<QString, JASPControlWrapper*>	_rowControlsMap;
	QMap<QString, QQmlContext*>			_contextMap;
	QMap<QString, QVariant>				_rowControlsVarMap;
	QList<QVariant>						_rowControls;
};

#endif // ROWCOMPONENTS_H
