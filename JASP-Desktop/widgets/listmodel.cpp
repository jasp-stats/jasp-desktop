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
#include "../analysis/analysisform.h"
#include "boundqmllistviewterms.h"
#include "rowcontrols.h"
#include "../analysis/jaspcontrolbase.h"

ListModel::ListModel(QMLListView* listView) 
	: QAbstractTableModel(listView)
	, _listView(listView)
{
	setInfoProvider(listView->form());
	_areTermsVariables = true;
}

QHash<int, QByteArray> ListModel::roleNames() const
{
	static QHash<int, QByteArray>	roles = QAbstractTableModel::roleNames();
	static bool						setMe = true;

	if(setMe)
	{
		roles[TypeRole]				= "type";
		roles[SelectedRole]			= "selected";
		roles[SelectableRole]		= "selectable";
		roles[ColumnTypeRole]		= "columnType";
		roles[NameRole]				= "name";
		roles[RowComponentsRole]	= "rowComponents";

		setMe = false;
	}

	return roles;
}

void ListModel::refresh()
{
	beginResetModel(); 
	endResetModel();
}

void ListModel::addError(const QString &error) const
{
	_listView->addError(error);
}

void ListModel::initTerms(const Terms &terms, const RowControlsOptions& allOptionsMap)
{
	beginResetModel();
	_terms.set(terms);
	_rowControlsOptions = allOptionsMap;
	endResetModel();
}

Terms ListModel::getSourceTerms()
{
	const QList<QMLListView::SourceType*>& sourceItems = listView()->sourceModels();

	Terms termsAvailable;
	if (sourceItems.size() == 0)
		return termsAvailable;

	for (QMLListView::SourceType* sourceItem : sourceItems)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			Terms terms = sourceModel->terms(sourceItem->modelUse);

			for (const QMLListView::SourceType& discardModel : sourceItem->discardModels)
				terms.discardWhatDoesContainTheseComponents(discardModel.model->terms(discardModel.modelUse));

			termsAvailable.add(terms);
		}
	}
	
	return termsAvailable;
}

QMap<ListModel*, Terms> ListModel::getSourceTermsPerModel()
{
	QMap<ListModel*, Terms> result;
	const QList<QMLListView::SourceType*>& sourceItems = listView()->sourceModels();

	if (sourceItems.size() == 0)
		return result;

	for (QMLListView::SourceType* sourceItem : sourceItems)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			Terms terms = sourceModel->terms(sourceItem->modelUse);

			for (const QMLListView::SourceType& discardModel : sourceItem->discardModels)
				terms.discardWhatDoesContainTheseComponents(discardModel.model->terms(discardModel.modelUse));

			result[sourceModel] = terms;
		}
	}

	return result;
}

void ListModel::setRowComponents(QVector<QQmlComponent *> &rowComponents)
{
	_rowComponents = rowComponents;
}

void ListModel::endResetModel()
{
	setUpRowControls();
	QAbstractTableModel::endResetModel();
}

void ListModel::setUpRowControls()
{
	if (_rowComponents.empty())
		return;

	int row = 0;
	for (const Term& term : terms())
	{
		const QString& key = term.asQString();
		if (!_rowControlsMap.contains(key))
			_rowControlsMap[key] = new RowControls(this, _rowComponents, _rowControlsOptions[key], row, key);
		else
			_rowControlsMap[key]->setContext(row, key);
		row++;
	}
}

int ListModel::searchTermWith(QString searchString)
{
	int result = -1;
	const Terms& myTerms = terms();
	int startIndex = 0;
	if (_selectedItems.length() > 0)
	{
		startIndex = _selectedItems.first();
		if (searchString.length() == 1)
			startIndex++;
	}

	if (searchString.length() > 0)
	{
		QString searchStringLower = searchString.toLower();
		for (size_t i = 0; i < myTerms.size(); i++)
		{
			size_t index = (size_t(startIndex) + i) % myTerms.size();
			const Term& term = myTerms.at(index);
			if (term.asQString().toLower().startsWith(searchStringLower))
			{
				result = int(index);
				break;
			}
		}
	}

	return result;
}

void ListModel::_addSelectedItemType(int _index)
{
	QString type = data(index(_index, 0), ListModel::ColumnTypeRole).toString();
	if (!type.isEmpty())
		_selectedItemsTypes.insert(type);
}

void ListModel::selectItem(int _index, bool _select)
{
	bool changed = false;
	if (_select)
	{
		if (data(index(_index, 0), ListModel::SelectableRole).toBool())
		{
			int i = 0;
			for (; i < _selectedItems.length(); i++)
			{
				if (_selectedItems[i] == _index)
					break;
				else if (_selectedItems[i] > _index)
				{
					_selectedItems.insert(i, _index);
					_addSelectedItemType(_index);
					changed = true;
					break;
				}
			}
			if (i == _selectedItems.length())
			{
				_selectedItems.append(_index);
				_addSelectedItemType(_index);
				changed = true;
			}
		}
	}
	else
	{
		if (_selectedItems.removeAll(_index) > 0)
		{
			_selectedItemsTypes.clear();
			for (int i : _selectedItems)
			{
				QString type = data(index(i, 0), ListModel::ColumnTypeRole).toString();
				if (!type.isEmpty())
					_selectedItemsTypes.insert(type);
			}
			changed = true;
		}
	}

	if (changed)
	{
		emit dataChanged(index(_index, 0), index(_index, 0));
		emit selectedItemsChanged();
	}
}

void ListModel::clearSelectedItems(bool emitSelectedChange)
{
	QList<int> selected = _selectedItems;

	_selectedItems.clear();
	_selectedItemsTypes.clear();

	for (int i : selected)
		emit dataChanged(index(i,0), index(i,0));

	if (selected.length() > 0 && emitSelectedChange)
		emit selectedItemsChanged();
}

void ListModel::setSelectedItem(int _index)
{
	clearSelectedItems(false);
	selectItem(_index, true);
}

void ListModel::selectAllItems()
{
	int nbTerms = int(terms().size());
	if (nbTerms == 0) return;

	_selectedItems.clear();
	_selectedItemsTypes.clear();

	for (int i = 0; i < nbTerms; i++)
	{
		if (data(index(i, 0), ListModel::SelectableRole).toBool())
		{
			_selectedItems.append(i);
			_addSelectedItemType(i);
		}
	}

	emit dataChanged(index(0, 0), index(nbTerms - 1, 0));
	emit selectedItemsChanged();
}


void ListModel::sourceTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	initTerms(getSourceTerms());
	
	emit modelChanged(termsAdded, termsRemoved);
}

int ListModel::rowCount(const QModelIndex &) const
{
	return int(terms().size());
}

QVariant ListModel::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	const Terms& myTerms = terms();
	size_t row_t = size_t(row);
	if (row_t >= myTerms.size())
		return QVariant();

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		const Term& term = myTerms.at(row_t);
		return QVariant(term.asQString());
	}
	if (role == ListModel::SelectableRole)
		return !myTerms.at(row_t).asQString().isEmpty();
	if (role == ListModel::SelectedRole)
	{
		if (_selectedItems.contains(row))
			return true;
		else
			return false;
	}
	if (role == ListModel::RowComponentsRole)
	{
		if (_rowControlsMap.size() > 0)
			return QVariant::fromValue(_rowControlsMap[myTerms.at(row_t).asQString()]->getControls());
		else
			return QVariant();
	}
	
	if (!areTermsVariables())
		return QVariant();
	
	if (role == ListModel::TypeRole)
		return QVariant("variable");
	else if (role == ListModel::ColumnTypeRole)
	{
		const Term& term = myTerms.at(row_t);
		if (term.size() != 1)
			return QVariant();

		return requestInfo(term, VariableInfo::VariableTypeName);
	}
	
	return QVariant();
}

const QString &ListModel::name() const
{
	return _listView->name();
}
