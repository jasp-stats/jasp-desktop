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

#include "listmodel.h"
#include "analysisforms/analysisqmlform.h"

#include <QTimer>
#include <QQmlProperty>

ListModel::ListModel(AnalysisQMLForm *form, QQuickItem *item) 
	: QAbstractListModel(form)
	, _item(item)
	, _form(form)
{
	setInfoProvider(form);
	_setAllowedVariablesToModel();
	_isSetUp = false;
	_removeTermsWhenDropped = true;
	
	_name = QQmlProperty(_item, "name").read().toString();
	_itemType = QQmlProperty(_item, "itemType").read().toString();
	_showVariableIcon = QQmlProperty(_item, "showVariableIcon").read().toBool();;
	
	QObject::connect(_item, SIGNAL(itemDoubleClicked(int)), this, SLOT(itemDoubleClickedHandler(int)));	
	QObject::connect(_item, SIGNAL(itemsDropped(QVariant, QVariant)), this, SLOT(itemsDroppedHandler(QVariant, QVariant)));		
}

void ListModel::setUp()
{
	if (!_isSetUp) // Cannot set the model in the constructor: it crashes.
		QQmlProperty(_item, "model").write(QVariant::fromValue(this));
	
	_isSetUp = true;		
}

void ListModel::setVariableTypesSuggested(int variableTypesSuggested) 
{
	_variableTypesSuggested = variableTypesSuggested;
}

int ListModel::variableTypesSuggested() const 
{
	return _variableTypesSuggested;
}

void ListModel::setVariableTypesAllowed(int variableTypesAllowed) 
{
	_variableTypesAllowed = variableTypesAllowed;
}

int ListModel::variableTypesAllowed() const 
{
	return _variableTypesAllowed;
}

bool ListModel::removeTermsWhenDropped() const
{
	return _removeTermsWhenDropped;
}

void ListModel::setRemoveTermsWhenDropped(bool remove)
{
	_removeTermsWhenDropped = remove;
}

void ListModel::refresh() {
	beginResetModel(); endResetModel();
}

QQuickItem *ListModel::getItem()
{
	return _item;
}

const QString &ListModel::getName() const
{
	return _name;
}

ListModel *ListModel::getRelatedModel()
{
	return _form->getRelatedModel(_item);
}

const QString &ListModel::getItemType() const
{
	return _itemType;
}

bool ListModel::showVariableIcon() const
{
	return _showVariableIcon;
}

void ListModel::setShowVariableIcon(bool show)
{
	_showVariableIcon = show;
	QQmlProperty::write(_item, "showVariableIcon", show);
}

QHash<int, QByteArray> ListModel::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[TypeRole] = "type";
	roles[NameRole] = "name";
	return roles;
}

int ListModel::rowCount(const QModelIndex &) const
{
	return _terms.size();
}

QVariant ListModel::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		Term term = _terms.at(row);
		return QVariant(term.asQString());
	}
	else if (role == ListModel::TypeRole && showVariableIcon())
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

Terms *ListModel::termsFromIndexes(const QList<int> &indexes) const
{
	Terms* terms = new Terms;
	foreach (const int &index, indexes)
	{
		Term term = _terms.at(index);
		terms->add(term);
	}
	
	return terms;
}

void ListModel::removeTermsAfterBeingDropped(const QList<int> &indexes)
{
	if (_removeTermsWhenDropped)
	{
		beginResetModel();
	
		QList<int> sorted = indexes;
		qSort(sorted.begin(), sorted.end(), qGreater<int>());
		foreach (const int &index, sorted)
			_terms.remove(index);
	
		endResetModel();
	}
}

const Terms &ListModel::terms() const
{
	return _terms;
}

bool ListModel::dropTerms(const Terms *terms)
{
	if (_removeTermsWhenDropped)
	{
		beginResetModel();
		_terms.add(*terms);
		endResetModel();
	}

	return true;
}

bool ListModel::canDropTerms(const Terms *terms) const
{
	foreach (const Term &term, *terms)
	{
		if ( ! isAllowed(term))
			return false;
	}

	return true;
}

bool ListModel::isAllowed(const Term &term) const
{
	if (_variableTypesAllowed == 0xff) return true;
	if (term.size() > 1) return true;
	
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	Column::ColumnType variableType = (Column::ColumnType)v.toInt();

	return variableType == 0 || variableType & _variableTypesAllowed;
}

bool ListModel::isSuggested(const Term &term) const
{
	if (_variableTypesSuggested == 0) return false;
	if (term.size() > 1) return false;
	
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	Column::ColumnType variableType = (Column::ColumnType)v.toInt();

	return variableType == 0 || variableType & _variableTypesSuggested;
}


void ListModel::addError(const QString &error)
{
	_form->addError(error);
}

int ListModel::_getAllowedColumnsTypes() const
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
				_form->addError(QString::fromLatin1("Wrong column type: ") + allowedColumn + (_name.isEmpty() ? QString() : (QString::fromLatin1(" for ListView ") + _name)));
		}
	}
	
	return allowedColumnsTypes;
}

void ListModel::_setAllowedVariablesToModel()
{
	_variableTypesSuggested = 0;
	_variableTypesAllowed = 0xff;
	
	int allowedColumnsTypes = _getAllowedColumnsTypes();
	
	if (allowedColumnsTypes >= 0)
		setVariableTypesAllowed(allowedColumnsTypes);
}



void ListModel::itemDoubleClickedHandler(int index)
{
	ListModel *targetModel = getRelatedModel();
	
	if (!targetModel)
	{
		_form->addError(QString::fromLatin1("No related item found") + (_name.isEmpty() ? QString() : (QString::fromLatin1(" for ListView ") + _name)));
		return;
	}
	QList<int> indexes;
	indexes.push_back(index);
	_moveItems(indexes, targetModel);
}

void ListModel::itemsDroppedHandler(QVariant vindexes, QVariant vdropList)
{
	QQuickItem* dropList = qobject_cast<QQuickItem*>(vdropList.value<QObject*>());
	ListModel* dropModel = NULL;
	
	if (!dropList)
		dropModel = getRelatedModel();
	else
	{
		QVariant vdropModel = QQmlProperty(dropList, "model").read();
		dropModel = qobject_cast<ListModel*>(vdropModel.value<QObject*>());
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
	// We need to move the items with another thread, if not, the drag and drop in QML get confused:
	// the call to itemsDropped is called from an item that will be removed (the items of the variable list
	// will be re-created). So itemsDropped should not call _moveItems directly.
	QTimer::singleShot(0, this, SLOT(moveItemsDelayedHandler()));
}

void ListModel::moveItemsDelayedHandler()
{
	_moveItems(_tempIndexes, _tempDropModel);
}

void ListModel::_moveItems(QList<int> &indexes, ListModel* targetModel)
{
	if (targetModel && indexes.size() > 0)
	{
		const Terms *terms = termsFromIndexes(indexes);	
		if (targetModel->canDropTerms(terms))
		{
			bool success = targetModel->dropTerms(terms);
			
			if (success && removeTermsWhenDropped())
				removeTermsAfterBeingDropped(indexes);
			else
				refresh();
		}
		else
		{
			qDebug() << "Cannot move items";
			refresh();
		}
		
		delete terms;
	}
	else
	{
		qDebug() << (!targetModel ? "no dropModel" : "no indexes");
	}
}
