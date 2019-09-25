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
		roles[TypeRole]			= "type";
		roles[ColumnTypeRole]	= "columnType";
		roles[NameRole]			= "name";
		roles[ExtraColumnsRole] = "extraColumns";

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

void ListModel::initTerms(const Terms &terms)
{
	beginResetModel();
	_terms.set(terms);
	endResetModel();
}

Terms ListModel::getSourceTerms()
{
	const QList<QMLListView::SourceType*>& sourceItems = listView()->sourceModels();
	
	Terms result;
	for (QMLListView::SourceType* sourceItem : sourceItems)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			const Terms& terms = sourceModel->terms();
			result.add(terms);
		}
	}
	
	return result;
}

void ListModel::sourceTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	initTerms(getSourceTerms());
	
	emit modelChanged(termsAdded, termsRemoved);
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
	
	if (!areTermsVariables())
		return QVariant();
	
	Term term = _terms.at(row);
	if (term.size() != 1)
		return QVariant();
	
	if (role == ListModel::TypeRole)
		return QVariant("variable");
	else if (role == ListModel::ColumnTypeRole)
	{
		QString variableTypeName = requestInfo(term, VariableInfo::VariableTypeName).toString();
		return QVariant(variableTypeName);
	}
	
	return QVariant();
}
