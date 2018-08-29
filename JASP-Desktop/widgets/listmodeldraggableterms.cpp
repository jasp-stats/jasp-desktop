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

#include "listmodeldraggableterms.h"
#include "analysis/analysisqmlform.h"

#include <QTimer>
#include <QQmlProperty>

ListModelDraggableTerms::ListModelDraggableTerms(AnalysisQMLForm *form, QQuickItem *item) 
	: ListModel(form, item)
	, _removeTermsWhenDragged(true)
	, _copyTermsWhenDropped(false)
{
	setInfoProvider(form);
	_setAllowedVariablesToModel();
	
	_itemType = QQmlProperty(_item, "itemType").read().toString();
	_areTermsVariables = QQmlProperty(_item, "showVariableIcon").read().toBool();
	QString dropMode = QQmlProperty(_item, "dropMode").read().toString();
	if (dropMode.isEmpty()) dropMode = "None";
	_dropMode = qmlDropModeFromQString(dropMode);
	
	QObject::connect(_item, SIGNAL(itemDoubleClicked(int)), this, SLOT(itemDoubleClickedHandler(int)));	
	QObject::connect(_item, SIGNAL(itemsDropped(QVariant, QVariant, int)), this, SLOT(itemsDroppedHandler(QVariant, QVariant, int)));		
}


void ListModelDraggableTerms::setVariableTypesSuggested(int variableTypesSuggested) 
{
	_variableTypesSuggested = variableTypesSuggested;
}

int ListModelDraggableTerms::variableTypesSuggested() const 
{
	return _variableTypesSuggested;
}

void ListModelDraggableTerms::setVariableTypesAllowed(int variableTypesAllowed) 
{
	_variableTypesAllowed = variableTypesAllowed;
}

int ListModelDraggableTerms::variableTypesAllowed() const 
{
	return _variableTypesAllowed;
}

void ListModelDraggableTerms::setDropMode(qmlDropMode dropMode)
{
	_dropMode = dropMode;
	QString dropModeStr = qmlDropModeToQString(dropMode);
	QQmlProperty::write(_item, "dropMode",dropModeStr);			
}

ListModelDraggableTerms *ListModelDraggableTerms::getRelatedModel()
{
	return _form->getRelatedModel(_item);
}

int ListModelDraggableTerms::rowCount(const QModelIndex &) const
{
	return _terms.size();
}

QVariant ListModelDraggableTerms::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole || role == ListModelDraggableTerms::NameRole)
	{
		Term term = _terms.at(row);
		return QVariant(term.asQString());
	}
	else if (role == ListModelDraggableTerms::TypeRole && areTermsVariables())
	{
		Term term = _terms.at(row);
		if (term.size() != 1) return QVariant();
		
		int variableType = requestInfo(term, VariableInfo::VariableType).toInt();

		switch (variableType)
		{
		case Column::ColumnTypeNominalText:
			return QVariant(QString("qrc:/icons/variable-nominal-text.svg"));
		case Column::ColumnTypeNominal:
			return QVariant(QString("qrc:/icons/variable-nominal.svg"));
		case Column::ColumnTypeOrdinal:
			return QVariant(QString("qrc:/icons/variable-ordinal.svg"));
		case Column::ColumnTypeScale:
			return QVariant(QString("qrc:/icons/variable-scale.svg"));
		default:
			return QVariant();
		}		
	}
	else
	{
		return QVariant();
	}
}

Terms *ListModelDraggableTerms::termsFromIndexes(const QList<int> &indexes) const
{
	Terms* terms = new Terms;
	for (uint index : indexes)
	{
		if (index < _terms.size())
		{
			Term term = _terms.at(index);
			terms->add(term);
		}
	}
	
	return terms;
}

void ListModelDraggableTerms::removeTerms(const QList<int> &indexes)
{
	beginResetModel();

	QList<int> sorted = indexes;
	qSort(sorted.begin(), sorted.end(), qGreater<int>());
	for (const int &index : sorted)
		_terms.remove(index);

	endResetModel();
}

void ListModelDraggableTerms::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	qmlDropMode _dropMode = dropMode();
	if (indexes.length() == 0 || _dropMode == qmlDropMode::None)
		return;	

	beginResetModel();
	Terms* terms = termsFromIndexes(indexes);
	removeTerms(indexes); // Remove first before adding: we cannot add terms that already exist
	for (int index : indexes)
	{
		if (index < dropItemIndex)
			dropItemIndex--;
	}
	Terms* removedTerms = addTerms(terms, dropItemIndex);
	if (removedTerms && removedTerms->size() > 0)
	{
		addTerms(removedTerms);
		delete removedTerms;
	}
	
	delete terms;
	endResetModel();
}

Terms* ListModelDraggableTerms::addTerms(Terms *terms, int dropItemIndex)
{
	Q_UNUSED(dropItemIndex);

	if (terms->size() > 0)
	{
		beginResetModel();
		_terms.add(*terms);
		endResetModel();
	}

	return NULL;
}

bool ListModelDraggableTerms::canAddTerms(Terms *terms) const
{
	for (const Term &term : *terms)
	{
		if ( ! isAllowed(term))
			return false;
	}

	return true;
}

bool ListModelDraggableTerms::isAllowed(const Term &term) const
{
	if (_variableTypesAllowed == 0xff) return true;
	if (term.size() > 1) return true;
	
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	Column::ColumnType variableType = (Column::ColumnType)v.toInt();

	return variableType == 0 || variableType & _variableTypesAllowed;
}

bool ListModelDraggableTerms::isSuggested(const Term &term) const
{
	if (_variableTypesSuggested == 0) return false;
	if (term.size() > 1) return false;
	
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	Column::ColumnType variableType = (Column::ColumnType)v.toInt();

	return variableType == 0 || variableType & _variableTypesSuggested;
}

int ListModelDraggableTerms::_getAllowedColumnsTypes() const
{
	int allowedColumnsTypes = -1;
	
	QStringList allowedColumns = QQmlProperty(_item, "allowedColumns").read().toStringList();
	if (allowedColumns.isEmpty())
	{
		QString allowedColumn = QQmlProperty(_item, "allowedColumns").read().toString();
		if (!allowedColumn.isEmpty())
			allowedColumns.append(allowedColumn);
	}
	if (!allowedColumns.isEmpty())
	{
		allowedColumnsTypes = 0;
		for (QString& allowedColumn: allowedColumns)
		{
			if (allowedColumn == "ordinal")
				allowedColumnsTypes |= Column::ColumnTypeOrdinal;
			else if (allowedColumn == "nominal")
			{
				allowedColumnsTypes |= Column::ColumnTypeNominal;
				allowedColumnsTypes |= Column::ColumnTypeNominalText;
			}
			else if (allowedColumn == "nominalText")
				allowedColumnsTypes |= Column::ColumnTypeNominalText;
			else if (allowedColumn == "nominalInt")
				allowedColumnsTypes |= Column::ColumnTypeNominal;
			else if (allowedColumn == "scale")
				allowedColumnsTypes |= Column::ColumnTypeScale;
			else
				addError(QString::fromLatin1("Wrong column type: ") + allowedColumn + (_name.isEmpty() ? QString() : (QString::fromLatin1(" for ListView ") + _name)));
		}
	}
	
	return allowedColumnsTypes;
}

void ListModelDraggableTerms::_setAllowedVariablesToModel()
{
	_variableTypesSuggested = 0;
	_variableTypesAllowed = 0xff;
	
	int allowedColumnsTypes = _getAllowedColumnsTypes();
	
	if (allowedColumnsTypes >= 0)
		setVariableTypesAllowed(allowedColumnsTypes);
}



void ListModelDraggableTerms::itemDoubleClickedHandler(int index)
{
	ListModelDraggableTerms *targetModel = getRelatedModel();
	
	if (!targetModel)
	{
		addError(QString::fromLatin1("No related item found") + (_name.isEmpty() ? QString() : (QString::fromLatin1(" for ListView ") + _name)));
		return;
	}
	QList<int> indexes;
	indexes.push_back(index);
	_moveItems(indexes, targetModel, -1);
}

void ListModelDraggableTerms::itemsDroppedHandler(QVariant vindexes, QVariant vdropList, int dropItemIndex)
{
	QQuickItem* dropList = qobject_cast<QQuickItem*>(vdropList.value<QObject*>());
	ListModelDraggableTerms* dropModel = NULL;
	
	if (!dropList)
		dropModel = getRelatedModel();
	else
	{
		QVariant vdropModel = QQmlProperty(dropList, "model").read();
		dropModel = qobject_cast<ListModelDraggableTerms*>(vdropModel.value<QObject*>());
	}
	
	if (!dropModel)
	{
		qDebug() << "No drop element!";
		return;
	}
	
	
	QList<QVariant> vvindexes = vindexes.toList();
	_tempIndexes.clear();
	for (QVariant &index : vvindexes)
		_tempIndexes.push_back(index.toInt());
	
	_tempDropModel = dropModel;
	_tempDropItemIndex = dropItemIndex;
	// We need to move the items with another thread, if not, the drag and drop in QML get confused:
	// the call to itemsDropped is called from an item that will be removed (the items of the variable list
	// will be re-created). So itemsDropped should not call _moveItems directly.
	QTimer::singleShot(0, this, SLOT(moveItemsDelayedHandler()));
}

void ListModelDraggableTerms::moveItemsDelayedHandler()
{
	_moveItems(_tempIndexes, _tempDropModel, _tempDropItemIndex);
}

void ListModelDraggableTerms::_moveItems(QList<int> &indexes, ListModelDraggableTerms* targetModel, int dropItemIndex)
{
	if (targetModel && indexes.size() > 0)
	{
		Options* options = _form->getAnalysisOptions();
		if (options != NULL)
			options->blockSignals(true);
		
		
		if (this == targetModel)
			moveTerms(indexes, dropItemIndex);
		else
		{
			bool success = true;		
			Terms* removedTermsWhenDropping = NULL;
			if (!copyTermsWhenDropped() && targetModel->removeTermsWhenDragged())
			{
				Terms* terms = termsFromIndexes(indexes);
				success = targetModel->canAddTerms(terms);
				if (success)
					removedTermsWhenDropping = targetModel->addTerms(terms, dropItemIndex);
				delete terms;
			}
				
			if (success && !targetModel->copyTermsWhenDropped() && removeTermsWhenDragged())
			{
				removeTerms(indexes);
				if (removedTermsWhenDropping)
				{
					if (removedTermsWhenDropping->size() > 0)
						addTerms(removedTermsWhenDropping);
					delete removedTermsWhenDropping;
				}
			}
			else
				refresh();
		}
		
		if (options != NULL)
			options->blockSignals(false);	
	}
	else
	{
		qDebug() << (!targetModel ? "no dropModel" : "no indexes");
	}
}
