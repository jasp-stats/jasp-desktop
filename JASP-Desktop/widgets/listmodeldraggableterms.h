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

#ifndef LISTMODELDRAGGABLETERMS_H
#define LISTMODELDRAGGABLETERMS_H

#include "listmodel.h"
#include "analysis/options/variableinfo.h"
#include "analysis/analysisqmldefines.h"

class AnalysisQMLForm;

class ListModelDraggableTerms : public ListModel, public VariableInfoConsumer
{	
Q_OBJECT
public:
	ListModelDraggableTerms(AnalysisQMLForm *form, QQuickItem* item);

	void setVariableTypesSuggested(int variableTypesSuggested);
	int variableTypesSuggested() const;

	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed() const;
	
	void setDropMode(qmlDropMode dropMode);
	qmlDropMode dropMode() const { return _dropMode; }
	
	bool copyTermsWhenDropped() const { return _copyTermsWhenDropped; }
	bool removeTermsWhenDragged() const { return _removeTermsWhenDragged; }
	
	ListModelDraggableTerms* getRelatedModel();
		
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	
	virtual Terms* termsFromIndexes(const QList<int> &indexes) const;
	virtual bool canAddTerms(Terms* terms) const;
	virtual Terms* addTerms(Terms* terms, int dropItemIndex = -1) ;
	virtual void removeTerms(const QList<int>& indexes);
	virtual void moveTerms(const QList<int>& indexes, int dropItemIndex = -1);
	
protected:
	int _variableTypesAllowed;
	int _variableTypesSuggested;
	bool _removeTermsWhenDragged;
	bool _copyTermsWhenDropped;
	qmlDropMode _dropMode;
		
	bool isAllowed(const Term &term) const;
	bool isSuggested(const Term &term) const;
	
	
private slots:
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList, int dropItemIndex);
	
private:
	QList<int> _tempIndexes;
	ListModelDraggableTerms* _tempDropModel;
	int _tempDropItemIndex;
	
	int _getAllowedColumnsTypes() const;
	void _setAllowedVariablesToModel();
	void _moveItems(QList<int> &indexes, ListModelDraggableTerms* dropModel, int dropItemIndex);	
};

#endif // LISTMODELDRAGGABLETERMS_H
