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
#include "controls/variableslistbase.h"
#include "log.h"
#include "controls/jaspcontrol.h"
#include "controls/factorsformbase.h"
#include "models/listmodelassignedinterface.h"

using namespace std;

ListModelFactorsForm::ListModelFactorsForm(JASPListControl* listView)
	: ListModel(listView)
{
	_factorsForm = qobject_cast<FactorsFormBase*>(listView);
	_needsSource = false;
	connect(_factorsForm, &FactorsFormBase::nestedChanged, this, &ListModelFactorsForm::nestedChangedHandler);
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


	if (row >= _factors.size())
	{
		Log::log()  << "Unknown row " << row << " in ListModelFactorsForm" << std::endl;
		return QVariant();
	}
	
	const Factor& factor = _factors[row];
	
	if (role == Qt::DisplayRole || role == ListModelFactorsForm::FactorNameRole)	return factor.name;
	else if (role == ListModelFactorsForm::FactorTitleRole)							return factor.title;
	
	return ListModel::data(index, role);
}

void ListModelFactorsForm::initFactors(const FactorVec &factors)
{
	beginResetModel();
	
	_factors.clear();
	ListModelAvailableInterface* availableModel = qobject_cast<ListModelAvailableInterface*>(_factorsForm->availableVariablesList()->model());
	if (availableModel) availableModel->clearAssignedModels();

	Terms newTerms;

	int index = 0;
	Terms previousTerms;
	for (Factor factor : factors)
	{
		if (_factorsForm->nested())
			factor.initTerms.setUndraggableTerms(previousTerms);
		_factors.push_back(factor);
		index++;
		previousTerms = factor.initTerms;
		newTerms.add(factor.initTerms);
	}
	
	_setTerms(newTerms);
	endResetModel();
}

int ListModelFactorsForm::countVariables() const
{
	int count = 0;
	for (const Factor& factor : _factors)
		count += factor.listView ? factor.listView->count() : factor.initTerms.size();

	return count;
}


Terms ListModelFactorsForm::filterTerms(const Terms& terms, const QStringList& filters)
{
	Terms result;

	if (filters.contains("title"))
		for (const Factor& factor : _factors)
			result.add(factor.title);
	else
		result = terms;

	return ListModel::filterTerms(result, filters);
}

void ListModelFactorsForm::addFactor()
{
	beginInsertRows(QModelIndex(), _factors.size(), _factors.size());

	QString index = QString::number(_factors.size() + _factorsForm->startIndex());
	QString name = _factorsForm->baseName() + index;
	QString title = _factorsForm->baseTitle() + " " + index;
	Terms terms;
	if (_factors.size() > 0 && _factorsForm->nested())
	{
		terms = _factors[_factors.size() - 1].listView->model()->terms();
		terms.setDraggable(false);
	}

	_factors.push_back(Factor(name, title, terms));

	endInsertRows();
	
}

void ListModelFactorsForm::removeFactor()
{
	if (_factors.size() > 1)
	{		
		JASPListControl* listView = _factors[_factors.size() - 1].listView;

		if (listView)
		{
			beginRemoveRows(QModelIndex(), _factors.size() - 1, _factors.size() - 1);

			const Terms& lastTerms = listView->model()->terms();
			_removeTerms(lastTerms);
			ListModelAvailableInterface* availableModel = qobject_cast<ListModelAvailableInterface*>(_factorsForm->availableVariablesList()->model());
			ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(listView->model());
			availableModel->removeAssignedModel(assignedModel);
			_factors.pop_back();

			endRemoveRows();
		}
		else
			Log::log() << "No list View found when removing factor" << std::endl;

	}
}

void ListModelFactorsForm::titleChangedSlot(int row, QString title)
{
	if (row < 0 && row >= _factors.size())
		return;
	
	if (_factors[row].title == title)
		return;

	_factors[row].title = title;

	emit dataChanged(index(row, 0), index(row,0));
}

void ListModelFactorsForm::resetModelTerms()
{
	if (_ensuringNesting) return;

	Terms allTerms;

	for (const Factor& factor : _factors)
	{
		JASPListControl* listView = factor.listView;
		allTerms.add(listView ? listView->model()->terms() : Terms(factor.initTerms));
	}

	_setTerms(allTerms);

	emit dataChanged(index(0,0), index(_factors.size() - 1, 0));
}

void ListModelFactorsForm::factorAdded(int index, VariablesListBase* listView)
{
	if (index >= _factors.size())
	{
		Log::log()  << "Factor added with wrong index: " << index << ". Max index: " << _factors.size() << std::endl;
		return;
	}
	
	Factor& factor = _factors[index];
	factor.listView = listView;
	Terms terms(factor.initTerms);

	ListModelDraggable* model = listView->draggableModel();

	model->initTerms(terms);
}

void ListModelFactorsForm::ensureNesting()
{
	if (_ensuringNesting || !_factorsForm->nested()) return;

	ListModelDraggable	*currentModel	= qobject_cast<ListModelDraggable*>(sender()),
						*onderModel		= nullptr,
						*upperModel		= nullptr;

	if (currentModel)
	{
		for (int i = 0; i < _factors.size(); i++)
		{
			if (currentModel == _factors[i].listView->model())
			{
				if (i > 0)
					upperModel = qobject_cast<ListModelDraggable*>(_factors[i-1].listView->model());
				if (i + 1 < _factors.size())
					onderModel = qobject_cast<ListModelDraggable*>(_factors[i+1].listView->model());
			}
		}
	}

	if (upperModel)
	{
		// Check first that the terms of the current model are correct: for example when adding a term, this term is made draggable in the VariablesList,
		// but if it exists also in the upper model, it should be set undraggable
		Terms newTerms = currentModel->terms();
		newTerms.setUndraggableTerms(upperModel->terms());
		if (!newTerms.strictlyEquals(currentModel->terms()))
		{
			// By changing the current model, avoid calling again the ensureNesting slot
			_ensuringNesting = true;
			currentModel->initTerms(newTerms);
			_ensuringNesting = false;
		}
	}

	if (onderModel)
	{
		Terms newTerms = onderModel->terms();
		newTerms.setUndraggableTerms(currentModel->terms());
		if (!newTerms.strictlyEquals(onderModel->terms()))
			onderModel->initTerms(newTerms);
	}

}


void ListModelFactorsForm::nestedChangedHandler()
{
	if (!_factorsForm->initialized())
		return;

	if (_factorsForm->nested())
	{
		for (int i = 0; i < _factors.size() - 1; i++)
		{
			ListModelDraggable	*currentModel = qobject_cast<ListModelDraggable*>(_factors[i].listView->model()),
								*onderModel = qobject_cast<ListModelDraggable*>(_factors[i+1].listView->model());

			Terms newTerms = onderModel->terms();
			newTerms.setUndraggableTerms(currentModel->terms());
			if (!newTerms.strictlyEquals(onderModel->terms()))
				onderModel->initTerms(newTerms);
		}
	}
	else
	{
		// Set all terms draggable
		for (int i = 0; i < _factors.size(); i++)
		{
			ListModelDraggable	*currentModel = qobject_cast<ListModelDraggable*>(_factors[i].listView->model());
			Terms draggableTerms = currentModel->terms();
			draggableTerms.setDraggable(true);
			if (!draggableTerms.strictlyEquals(currentModel->terms()))
				currentModel->initTerms(draggableTerms);
		}
	}
}
