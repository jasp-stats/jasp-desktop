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
#include <json/json.h>

class JASPListControl;
class ListModel;
class JASPControl;
class Option;
class Term;

class RowControls : public QObject
{
Q_OBJECT

public:
	
	RowControls(
			ListModel* parent
			, QQmlComponent* components
			, const QMap<QString, Json::Value>& rowValues);

	void										init(int row, const Term& key, bool isNew);
	void										setContext(int row, const QString& key);
	QQmlComponent*								getComponent()								const	{ return _rowComponent; }
	QQuickItem*									getRowObject()								const	{ return _rowObject;			}
	const QMap<QString, JASPControl*>&			getJASPControlsMap()						const	{ return _rowJASPControlMap;	}
	JASPControl*								getJASPControl(const QString& name)					{ return _rowJASPControlMap.contains(name) ? _rowJASPControlMap[name] : nullptr; }
	bool										addJASPControl(JASPControl* control);
	void										disconnectControls();

private:

	void										_initializeControls(bool useInitialValue = true);

	ListModel*								_parentModel;
	QQmlComponent*							_rowComponent	= nullptr;
	QQuickItem*								_rowObject		= nullptr;
	QMap<QString, JASPControl*>				_rowJASPControlMap;
	QQmlContext*							_context;
	QMap<QString, Json::Value>				_initialValues;
};

#endif // ROWCOMPONENTS_H
