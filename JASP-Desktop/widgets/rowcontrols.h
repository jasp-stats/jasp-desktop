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
#include <QQuickItem>

class QMLListView;
class ListModel;
class JASPControlWrapper;
class Option;
class Term;

class RowControls : public QObject
{
Q_OBJECT

public:
	
	RowControls(
			ListModel* parent
			, QList<QQmlComponent *>& components
			, const QMap<QString, Option*>& rowOptions
			, bool isDummy = false);

	void										init(int row, const Term& key, bool isNew);
	void										setContext(int row, const QString& key);
	const QList<QVariant>&						getObjects()								const	{ return _rowObjects;			}
	const QMap<QString, JASPControlWrapper*>&	getJASPControlsMap()						const	{ return _rowJASPWrapperMap;	}
	bool										addJASPControl(JASPControlWrapper* control);

private:

	ListModel*								_parentModel;
	QList<QQmlComponent *>					_rowComponents;
	QList<QVariant>							_rowObjects;
	QMap<QString, JASPControlWrapper*>		_rowJASPWrapperMap;
	std::map<QQuickItem*, QQmlContext*>		_contextMap;
	QMap<QString, QVariant>					_rowControlsVarMap;
	QMap<QString, Option*>					_rowOptions;
	bool									_isDummy = false;
};

#endif // ROWCOMPONENTS_H
