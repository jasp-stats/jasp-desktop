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

#include "listmodelfactorsform.h"
#include "analysis/options/options.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "utilities/qutils.h"
#include "boundqmllistviewterms.h"

#include <QQuickItem>
#include <QDebug>

using namespace std;

ListModelFactorsForm::ListModelFactorsForm(QMLListView* listView)
	: ListModel(listView)
{
}

QHash<int, QByteArray> ListModelFactorsForm::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[FactorNameRole] = "factorName";
	roles[FactorTitleRole] = "factorTitle";
	return roles;
}

QVariant ListModelFactorsForm::data(const QModelIndex &index, int role) const
{
	int row = index.row();


	if (row >= _factors.length())	
	{
		qDebug() << "Unknown row " << row << " in ListModelFactorsForm";
		return QVariant();
	}
	
	Factor* factor = _factors[row];
	
	QVariant value;
	if (role == Qt::DisplayRole || role == ListModelFactorsForm::FactorNameRole)
	{
		value = factor->name;
	}
	else if (role == ListModelFactorsForm::FactorTitleRole)
	{
		value = factor->title;
	}
	
	return value;	
}

void ListModelFactorsForm::initFactors(const vector<tuple<string, string, vector<string> > > &factors)
{
	beginResetModel();
	
	_factors.clear();
	_terms.clear();
	int index = 0;
	for (const tuple<string, string, vector<string> > &factorTuple : factors)
	{
		QString name = tq(get<0>(factorTuple));
		Factor* factor = new Factor(name, tq(get<1>(factorTuple)), get<2>(factorTuple));
		_factors.push_back(factor);
        _terms.add(factor->name);
		index++;
	}
	
	endResetModel();	
}


const Terms& ListModelFactorsForm::terms() const
{
	return _terms;
}

vector<tuple<string, string, vector<string> > > ListModelFactorsForm::getFactors() const
{
	vector<tuple<string, string, vector<string> > > result;
	
	for (int i = 0; i < _factors.length(); i++)
	{
		Factor* factor = _factors[i];
		BoundQMLListViewTerms* listView = factor->listView;
		if (listView)
			result.push_back(make_tuple(factor->name.toStdString(), factor->title.toStdString(), listView->model()->terms().asVector()));
	}
	
	return result;
}

void ListModelFactorsForm::addFactor()
{
	beginResetModel();
	QString index = QString::number(_factors.size() + 1);
	QString name = tq("Factor") + index;
	QString title = tq("Factor ") + index;
	Factor* factor = new Factor(name, title);
	_factors.push_back(factor);
    _terms.add(name);
	endResetModel();
	
	emit modelChanged();
}

void ListModelFactorsForm::removeFactor()
{
	if (_factors.size() > 1)
	{
		beginResetModel();
		_factors.removeLast();
		_terms.remove(_terms[_terms.size() - 1]);
		endResetModel();
	}
	
	emit modelChanged();
}

void ListModelFactorsForm::titleChangedSlot(int index, QString title)
{
	if (index < 0 && index >= _factors.length())
		return;
	
	if (_factors[index]->title == title)
		return;
	
	_factors[index]->title = title;
	_terms.clear();
	
	for (Factor* factor : _factors)
        _terms.add(factor->name);
	
	emit modelChanged();
}

void ListModelFactorsForm::factorAddedSlot(int index, QVariant item)
{
	if (index >= _factors.length())
	{
		qDebug() << "Factor added with wrong index: " << index << ". Max index: " << _factors.length();
		return;
	}
	
	QQuickItem *quickItem = qobject_cast<QQuickItem *>(item.value<QObject *>());
	if (!quickItem)
	{
		qDebug() << "No quick Item found in factorAdded!";
		return;
	}
	
	Factor* factor = _factors[index];
	
	if (factor->listView)
		factor->listView->resetQMLItem(quickItem);
	else
	{
		factor->listView = new BoundQMLListViewTerms(quickItem, listView()->form());
		Terms terms(factor->initTerms);
		ListModelAssignedInterface* model = factor->listView->assignedModel();
		model->initTerms(terms);
		model->setCopyTermsWhenDropped(true);
		emit addListView(factor->listView);
	}
	
	factor->listView->setDropMode(qmlDropMode::Replace);		
	factor->listView->setUp();
}
