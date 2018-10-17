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
#include "analysis/analysisqmlform.h"

ListModel::ListModel(QMLListView* listView) 
	: QAbstractListModel(listView)
	, _listView(listView)
{
	setInfoProvider(listView->form());
	_areTermsVariables = true;
}

QHash<int, QByteArray> ListModel::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[TypeRole] = "type";
	roles[NameRole] = "name";
	return roles;
}

void ListModel::refresh() {
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

void ListModel::syncTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	const QList<ListModel*>& syncModels = listView()->syncModels();
	if (syncModels.empty())
		return;
	
	Terms tempTerms;
	for (ListModel* syncModel : syncModels)
	{
		const Terms& terms = syncModel->terms();
		tempTerms.add(terms);
	}
	
	initTerms(tempTerms);
	
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
	else if (role == ListModel::TypeRole && areTermsVariables())
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
