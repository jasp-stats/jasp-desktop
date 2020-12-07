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
#include "variableslistbase.h"
#include "log.h"
#include <QQuickItem>
#include "analysis/jaspcontrol.h"
#include "analysis/analysisform.h"


using namespace std;

ListModelFactorsForm::ListModelFactorsForm(JASPListControl* listView)
	: ListModel(listView)
{
	setTermsAreVariables(false);
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
		Log::log()  << "Unknown row " << row << " in ListModelFactorsForm" << std::endl;
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
	_titles.clear();
	Terms newTerms;

	int index = 0;
	for (const tuple<string, string, vector<string> > &factorTuple : factors)
	{
		QString name = tq(get<0>(factorTuple));
		QString title = tq(get<1>(factorTuple));
		std::vector<string> terms = get<2>(factorTuple);
		Factor* factor = new Factor(name, title, terms);
		_factors.push_back(factor);
        _titles.add(title);
		newTerms.add(Terms(terms));
		index++;
	}
	
	_setTerms(newTerms);
	endResetModel();	
}


Terms ListModelFactorsForm::termsEx(const QString& what)
{
	if (what == "title")
		return _titles;
	return ListModel::termsEx(what);
}

vector<tuple<string, string, vector<string> > > ListModelFactorsForm::getFactors() const
{
	vector<tuple<string, string, vector<string> > > result;
	
	for (int i = 0; i < _factors.length(); i++)
	{
		Factor* factor = _factors[i];
		JASPListControl* listView = factor->listView;
		if (listView)
			result.push_back(make_tuple(factor->name.toStdString(), factor->title.toStdString(), listView->model()->terms().asVector()));
	}
	
	return result;
}

void ListModelFactorsForm::addFactor()
{
	beginInsertRows(QModelIndex(), _factors.size(), _factors.size());

	QString index = QString::number(_factors.size() + 1);
	QString name = tq("Factor") + index;
	QString title = tq("Factor ") + index;
	Factor* factor = new Factor(name, title);
	_factors.push_back(factor);
    _titles.add(title);

	endInsertRows();
	
}

void ListModelFactorsForm::removeFactor()
{
	if (_factors.size() > 1)
	{		
		JASPListControl* listView = _factors[_factors.size() - 1]->listView;

		if (listView)
		{
			beginRemoveRows(QModelIndex(), _factors.size() - 1, _factors.size() - 1);

			const Terms& lastTerms = listView->model()->terms();
			_removeTerms(lastTerms);
			_titles.remove(_titles.size() - 1);
			_factors.removeLast();

			endRemoveRows();
		}
		else
			Log::log() << "No list View found when removing factor" << std::endl;

	}
	
}

void ListModelFactorsForm::titleChangedSlot(int index, QString title)
{
	if (index < 0 && index >= _factors.length())
		return;
	
	if (_factors[index]->title == title)
		return;

	beginResetModel();

	_factors[index]->title = title;
	_titles.clear();
	
	for (Factor* factor : _factors)
        _titles.add(factor->title);

	endResetModel();
}

void ListModelFactorsForm::factorAddedSlot(int index, QVariant item)
{
	if (index >= _factors.length())
	{
		Log::log()  << "Factor added with wrong index: " << index << ". Max index: " << _factors.length() << std::endl;
		return;
	}
	
	VariablesListBase* listView = qobject_cast<VariablesListBase *>(item.value<QObject *>());
	if (!listView)
	{
		Log::log() << "JASP Control is not a VariablesListBase in factorAdded" << std::endl;
		return;
	}
	
	Factor* factor = _factors[index];
	factor->listView = listView;
	Terms terms(factor->initTerms);
	ListModelDraggable* model = listView->draggableNodel();
	model->setInfoProvider(listView->form());
	model->initTerms(terms);
	model->setCopyTermsWhenDropped(true);
	connect(model, &ListModelDraggable::modelReset, this, &ListModelFactorsForm::resetTerms);
	emit addListView(factor->listView);
	
	factor->listView->setUp();
}

void ListModelFactorsForm::resetTerms()
{
	beginResetModel();

	Terms newTerms = terms();
	for (Factor* factor : _factors)
	{
		if (factor->listView)
		{
			const Terms& terms = factor->listView->model()->terms();
			newTerms.add(terms);
			factor->initTerms = terms.asVector();
		}
	}

	_setTerms(newTerms);
	
	endResetModel();
}
