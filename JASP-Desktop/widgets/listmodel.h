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

#ifndef LISTMODEL_H
#define LISTMODEL_H

#include "common.h"
#include "analysis/options/terms.h"

#include <QAbstractListModel>
#include <QQuickItem>

class AnalysisQMLForm;

class ListModel : public QAbstractListModel
{
	Q_OBJECT
public:
	enum ListModelRoles {
        NameRole = Qt::UserRole + 1,
		TypeRole
    };
	
	ListModel(AnalysisQMLForm* form, QQuickItem* item);
	virtual QHash<int, QByteArray> roleNames() const OVERRIDE;
	
	virtual void setUp();
	virtual void refresh();
	void addError(const QString& error) const;
	QQuickItem* item() const			{ return _item; }
	const QString& name() const			{ return _name; }
	virtual const Terms& terms() const	{ return _terms; }
	const QString& getItemType() const	{ return _itemType; }	
	bool areTermsVariables() const		{ return _areTermsVariables; }
	virtual void setTermsAreNotVariables();
	
	
signals:
	void termsChanged(Terms* added = NULL, Terms* removed = NULL);
	
protected:
	AnalysisQMLForm* _form;
	QQuickItem* _item;
	QString _name;
	bool _isSetUp;
	Terms _terms;
	QString _itemType;	
	bool _areTermsVariables;	
};

#endif // LISTMODEL_H
