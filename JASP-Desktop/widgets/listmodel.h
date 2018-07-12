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

#include <QAbstractListModel>
#include <QQuickItem>
#include "terms.h"
#include "common.h"
#include "variableinfo.h"

class AnalysisQMLForm;

class ListModel : public QAbstractListModel, public VariableInfoConsumer
{	
Q_OBJECT
public:
	enum ListModelRoles {
        NameRole = Qt::UserRole + 1,
        TypeRole
    };
	
	ListModel(AnalysisQMLForm *form, QQuickItem* item);
	virtual void setUp();

	void setVariableTypesSuggested(int variableTypesSuggested);
	int variableTypesSuggested() const;

	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed() const;
	
	bool removeTermsWhenDropped() const;
	void setRemoveTermsWhenDropped(bool remove);
	
	void refresh();
	
	QQuickItem* getItem();
	const QString& getName() const;
	ListModel* getRelatedModel();
	const QString& getItemType() const;
	bool showVariableIcon() const;
	void setShowVariableIcon(bool show);
		
	virtual QHash<int, QByteArray> roleNames() const OVERRIDE;
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	
	virtual Terms *termsFromIndexes(const QList<int> &indexes) const;
	virtual bool canDropTerms(const Terms *terms) const;
	virtual bool dropTerms(const Terms *terms) ;
	virtual void removeTermsAfterBeingDropped(const QList<int> &indexes);
	virtual const Terms& terms() const;

signals:
	void termsChanged(Terms* added = NULL, Terms* removed = NULL);

protected:
	QQuickItem* _item;
	AnalysisQMLForm* _form;
	QString _name;
	QString _itemType;
	int _variableTypesAllowed;
	int _variableTypesSuggested;
	bool _removeTermsWhenDropped;
	bool _showVariableIcon;
	
	Terms _terms;

	
	void addError(const QString& error);
	bool isAllowed(const Term &term) const;
	bool isSuggested(const Term &term) const;
	
	
private slots:
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList);
	
private:
	QList<int> _tempIndexes;
	ListModel* _tempDropModel;
	bool _isSetUp;
	
	int _getAllowedColumnsTypes() const;
	void _setAllowedVariablesToModel();
	void _moveItems(QList<int> &indexes, ListModel* dropModel);	
};

#endif // LISTMODEL_H
