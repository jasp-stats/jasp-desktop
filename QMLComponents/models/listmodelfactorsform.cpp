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
#include "utilities/qutils.h"
#include "controls/variableslistbase.h"
#include "log.h"
#include "controls/jaspcontrol.h"
#include "analysisform.h"


using namespace std;

ListModelFactorsForm::ListModelFactorsForm(JASPListControl* listView)
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
		Log::log()  << "Unknown row " << row << " in ListModelFactorsForm" << std::endl;
		return QVariant();
	}
	
	Factor* factor = _factors[row];
	
	if (role == Qt::DisplayRole || role == ListModelFactorsForm::FactorNameRole)	return factor->name;
	else if (role == ListModelFactorsForm::FactorTitleRole)							return factor->title;
	
	return ListModel::data(index, role);
}

void ListModelFactorsForm::initFactors(const FactorVec &factors)
{
	beginResetModel();
	
	_factors.clear();
	Terms newTerms;

	int index = 0;
	for (const auto &factorTuple : factors)
	{
		QString name = tq(get<0>(factorTuple));
		QString title = tq(get<1>(factorTuple));
		std::vector<string> terms = get<2>(factorTuple);
		Factor* factor = new Factor(name, title, terms);
		_factors.push_back(factor);
		newTerms.add(Terms(terms));
		index++;
	}
	
	_setTerms(newTerms);
	endResetModel();
}

int ListModelFactorsForm::countVariables() const
{
	int count = 0;
	for (Factor* factor : _factors)
		count += factor->listView->count();

	return count;
}


Terms ListModelFactorsForm::filterTerms(const Terms& terms, const QStringList& filters)
{
	Terms result;

	if (filters.contains("title"))
		for (Factor* factor : _factors)
			result.add(factor->title);
	else
		result = terms;

	return ListModel::filterTerms(result, filters);
}

ListModelFactorsForm::FactorVec ListModelFactorsForm::getFactors()
{
	ListModelFactorsForm::FactorVec result;
	
	for (Factor* factor : _factors)
	{
		JASPListControl* listView = factor->listView;
		if (listView)
		{
			Terms terms = listView->model()->terms();
			result.push_back(make_tuple(fq(factor->name), fq(factor->title), terms.asVector()));
		}
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
			_factors.removeLast();

			endRemoveRows();
		}
		else
			Log::log() << "No list View found when removing factor" << std::endl;

	}
}

void ListModelFactorsForm::titleChangedSlot(int row, QString title)
{
	if (row < 0 && row >= _factors.length())
		return;
	
	if (_factors[row]->title == title)
		return;

	_factors[row]->title = title;

	emit dataChanged(index(row, 0), index(row,0));
}

void ListModelFactorsForm::resetModelTerms()
{
	Terms allTerms;

	for (Factor* factor : _factors)
	{
		JASPListControl* listView = factor->listView;
		if (listView)
			allTerms.add(listView->model()->terms());
	}

	_setTerms(allTerms);

	emit dataChanged(index(0,0), index(_factors.length() - 1, 0));
}

void ListModelFactorsForm::factorAdded(int index, VariablesListBase* listView)
{
	if (index >= _factors.length())
	{
		Log::log()  << "Factor added with wrong index: " << index << ". Max index: " << _factors.length() << std::endl;
		return;
	}
	
	Factor* factor = _factors[index];
	factor->listView = listView;
	Terms terms(factor->initTerms);
	ListModelDraggable* model = listView->draggableModel();
	model->setCopyTermsWhenDropped(true);
	model->initTerms(terms);
}
