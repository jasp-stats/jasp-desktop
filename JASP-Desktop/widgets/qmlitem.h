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

#ifndef QMLITEM_H
#define QMLITEM_H

#include <QString>
#include <QVector>
#include <QVariant>

class QQuickItem;
class AnalysisForm;

class QMLItem
{

public:
			 QMLItem(QQuickItem* item, AnalysisForm* form);
	virtual ~QMLItem() {}

	virtual void				setUp() {}
	virtual void				cleanUp();
	virtual void				resetQMLItem(QQuickItem* item);

	const QString			&	name()		const { return _name; }
	AnalysisForm			*	form()		const { return _form; }
	QQuickItem				*	item()		const { return _item; }
	const QVector<QMLItem*>	&	depends()	const { return _depends; }
	void						addError(const QString& error);
	bool						addDependency(QMLItem* item);
	void						removeDependency(QMLItem* item);
	QVariant					getItemProperty(const QString& name);
	void						setItemProperty(const QString& name, const QVariant& value);
	void						showControlError(QString msg);
	void						showControlErrorTemporary(QString msg);

protected:
	
	QQuickItem*				_item;
	QString					_name;
	AnalysisForm*			_form;
	QVector<QMLItem*>		_depends;
};

#endif // QMLITEM_H
