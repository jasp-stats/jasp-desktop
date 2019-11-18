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
	for (const Term& term : _terms)
	{
		const QString& key = term.asQString();
		if (!_rowControlsMap.contains(key))
			_rowControlsMap[key] = new RowControls(this, _rowComponents, _rowControlsOptions[key], row, key);
		else
			_rowControlsMap[key]->setContext(row, key);
		row++;
	}
}


void ListModel::sourceTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	initTerms(getSourceTerms());
	
	emit modelChanged(termsAdded, termsRemoved);
}

int ListModel::rowCount(const QModelIndex &) const
{
	return int(_terms.size());
}

QVariant ListModel::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	size_t row_t = size_t(row);
	if (row_t >= _terms.size())
		return QVariant();

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		const Term& term = _terms.at(row_t);
		return QVariant(term.asQString());
	}
	if (role == ListModel::RowComponentsRole)
	{
		if (_rowControlsMap.size() > 0)
			return QVariant::fromValue(_rowControlsMap[_terms.at(row_t).asQString()]->getControls());
		else
			return QVariant();
	}
	
	if (!areTermsVariables())
		return QVariant();
	
	if (role == ListModel::TypeRole)
		return QVariant("variable");
	else if (role == ListModel::ColumnTypeRole)
	{
		Term term = _terms.at(row_t);
		if (term.size() != 1)
			return QVariant();

		QString variableTypeName = requestInfo(term, VariableInfo::VariableTypeName).toString();
		return QVariant(variableTypeName);
	}
	
	return QVariant();
}

const QString &ListModel::name() const
{
	return _listView->name();
}
