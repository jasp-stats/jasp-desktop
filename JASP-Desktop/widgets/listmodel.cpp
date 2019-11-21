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
#include "listmodelextracontrols.h"
#include "boundqmllistviewterms.h"
#include <QTimer>

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

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		Term term = _terms.at(row);
		return QVariant(term.asQString());
	}

	if (role == ListModel::ExtraColumnsRole && _extraControlsModels.count() > 0)
	{
		int row = index.row();
		return QVariant::fromValue(_extraControlsModels[_rowNameMap[row]]);
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

void ListModel::endResetModel()
{
	addExtraControlModels();
	QAbstractTableModel::endResetModel();
}

const QString &ListModel::name() const
{
	return _listView->name();
}

void ListModel::addExtraControlModels()
{
	if (!_extraControlsDefinitions.isEmpty())
	{
		_extraControlsModels.clear();
		_rowNameMap.clear();
		for (int i = 0; i < rowCount(); i++)
		{
			QString colName = data(index(i, 0), ListModel::NameRole).toString();
			_rowNameMap[i] = colName;
			if (_modelCache.contains(colName))
				_extraControlsModels[colName] = _modelCache[colName];
			else
			{
				ListModelExtraControls* extraControlsModel = new ListModelExtraControls(this, _extraControlsDefinitions);
				_extraControlsModels[colName] = extraControlsModel;
				_modelCache[colName] = extraControlsModel;
			}
		}
	}
}

// This function is called by initTerms for models that may have extra columns
void ListModel::initExtraControlTerms()
{
	if (!_extraControlsDefinitions.isEmpty())
		// initTerms calls begin/endResetModel that will build the QML items in the List View.
		// We must wait that these QML Items are completely built so that we can bind the extra controls (if they exist)
		QTimer::singleShot(0, this, &ListModel::_initExtraControlTerms);
}

void ListModel::_initExtraControlTerms()
{
	BoundQMLItem* boundItem = dynamic_cast<BoundQMLItem*>(listView());
	if (boundItem)
		boundItem->bindExtraControlOptions();
}


void ListModel::addExtraControls(const QVector<QMap<QString, QVariant> > &extraControlColumns)
{
	_extraControlsDefinitions = extraControlColumns;
}
