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
#include "analysis/options/variableinfo.h"
#include "analysis/options/terms.h"
#include "qmllistview.h"

#include <QAbstractListModel>

class ListModel : public QAbstractTableModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	enum ListModelRoles {
        NameRole = Qt::UserRole + 1,
		TypeRole,
		ColumnTypeRole,
		ExtraColumnsRole
    };

	ListModel(QMLListView* listView);
	virtual QHash<int, QByteArray> roleNames() const OVERRIDE;

	QMLListView* listView() const							{ return _listView; }
	const QString& name() const								{ return _listView->name(); }
	virtual const Terms& terms() const						{ return _terms; }
	bool areTermsVariables() const							{ return _areTermsVariables; }
	bool areTermsInteractions() const						{ return _areTermsInteractions; }
	virtual QString getItemType(const Term& term) const		{ return _itemType; }	
	void setTermsAreVariables(bool areVariables)			{ _areTermsVariables = areVariables; }
	void setTermsAreInteractions(bool interactions)			{ _areTermsInteractions = interactions; }
	void setItemType(QString type)							{ _itemType = type; }

	void addError(const QString& error) const;

	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE { return 1; }
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;	

	virtual void refresh();
	virtual void initTerms(const Terms &terms);
	virtual Terms getSourceTerms();

signals:
	void modelChanged(Terms* added = nullptr, Terms* removed = nullptr);

public slots:	
	virtual void sourceTermsChanged(Terms* termsAdded, Terms* termsRemoved);

protected:
	QMLListView* _listView;
	QString _itemType;
	Terms _terms;
	bool _areTermsVariables;
	bool _areTermsInteractions = false;
};

#endif // LISTMODEL_H
