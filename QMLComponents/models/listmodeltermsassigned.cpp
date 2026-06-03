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

#include "listmodeltermsassigned.h"
#include "listmodeltermsavailable.h"
#include "analysisform.h"
#include "controls/rowcontrols.h"
#include "controls/jasplistcontrol.h"
#include <QTimer>


using namespace std;

ListModelTermsAssigned::ListModelTermsAssigned(JASPListControl* listView)
	: ListModelAssignedInterface(listView)
{
}

void ListModelTermsAssigned::initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap)
{
    ListModelAssignedInterface::initTerms(terms, allValuesMap);

	if (availableModel())
		availableModel()->removeTermsInAssignedList();
}

void ListModelTermsAssigned::availableTermsResetHandler(Terms termsAdded, Terms termsRemoved)
{
	if (termsAdded.size() > 0 && listView()->addAvailableVariablesToAssigned())
	{
		beginResetModel();
		_addTerms(termsAdded);
		endResetModel();

		if (availableModel())
			availableModel()->removeTermsInAssignedList();
	}

	if (termsRemoved.size() > 0)
	{
		beginResetModel();
		_removeTerms(termsRemoved);
		endResetModel();
	}
}

Terms ListModelTermsAssigned::canAddTerms(const Terms& terms) const
{
	if (listView()->maxRows() >= 0 && int(terms.size()) > listView()->maxRows())
		return Terms();

	return ListModelDraggable::canAddTerms(terms);
}

Terms ListModelTermsAssigned::addTerms(const Terms& termsToAdd, int dropItemIndex, const Terms::RelatedValuesPerTerm& rowValues)
{
	Terms termsToSendBack;
	int maxRows = listView()->maxRows(); // maxRows == -1 means no maximum

	if (termsToAdd.size() == 0)
		return termsToSendBack;
	else if (maxRows > 0 && dropItemIndex >= maxRows)
		return termsToAdd;


	Terms termsToAddWithRightTypes = termsToAdd;
	for (Term & term : termsToAddWithRightTypes)
	{
		// If the real type is now allowed, change the type to its original one.
		columnType realType = getVariableRealType(term.value());
		if (term.type() != realType && listView()->isTypeAllowed(realType))
			term.setType(realType);
	}


	if (dropItemIndex < 0 && maxRows == 1)
		dropItemIndex = 0; // for single row, per default replace old item by new one.

	for (const auto& it : rowValues.toStdMap())
		_rowControlsValues[it.first] = it.second;

	if (dropItemIndex == 0 && maxRows == termsToAdd.size())
	{
		// If we replace all the items, use beginResetModel
		termsToSendBack = terms();

		beginResetModel();
		_setTerms(termsToAddWithRightTypes);
		endResetModel();
	}
	else
	{
		// We try to use beginInsertRows/endInsetRows (and beginRemoveRows/endRemoveRows) to set the values instead of beginResetModel: this is indeed the right way to use QAbstractItemModel
		// By using beginResetModel/endResetModel all QML objects of the list are removed and rebuild again. This should not be a problem, apart from one special case:
		// in a TabView, if the user changes the title of a Tab and clicks direclty the '+' button to add another tab, adding a new tab will be done first, and will add a new
		// term to the model: if the model of the TabView is reset, the TextField controls that handle the titles of the Tabs are destroyed and recreated. As the TextField control
		// that was used to change the title is destroyed, the signal that changes this title is not received, and the title gets back its old value.
		if (dropItemIndex < 0 || dropItemIndex > terms().size())
			dropItemIndex = terms().size();

		beginInsertRows(QModelIndex(), dropItemIndex, dropItemIndex + termsToAdd.size() - 1);
		Terms newTerms = terms();
		if (dropItemIndex < terms().size())
		{
			newTerms.insert(dropItemIndex, termsToAddWithRightTypes);
			_setTerms(newTerms);
		}
		else
		{
			newTerms.add(termsToAddWithRightTypes);
			_addTerms(termsToAddWithRightTypes);
		}

		endInsertRows();

		if (maxRows > 0 && newTerms.size() > maxRows)
		{
			for (size_t i = maxRows; i < newTerms.size(); i++)
				termsToSendBack.add(newTerms.at(i));
			newTerms.remove(maxRows, newTerms.size() - maxRows);

			beginRemoveRows(QModelIndex(), maxRows, maxRows + termsToSendBack.size());
			_setTerms(newTerms);
			endRemoveRows();

			listView()->addControlWarningTemporary(tr("Only %1 variables are allowed").arg(maxRows));
		}
	}

	return termsToSendBack;
}

void ListModelTermsAssigned::removeTerm(int index)
{
	if (index < 0 || index >= rowCount()) return;

	beginRemoveRows(QModelIndex(), index, index);

	const Term& term = terms().at(size_t(index));
	AnalysisForm* form = listView()->form();

	RowControls* controls = _rowControlsMap.value(term.value());
	if (controls)
	{
		for (JASPControl* control : controls->getJASPControlsMap().values())
		{
			control->setHasError(false);
			if (form)
				form->clearControlError(control);
		}

		_rowControlsMap.remove(term.value());
	}
	_removeTerm(term);

	endRemoveRows();
}

void ListModelTermsAssigned::changeTerm(int index, const Term& term)
{
	Term oldTerm = terms().at(index);
	if (oldTerm.value() != term.value())
	{
		_rowControlsMap[term.value()] = _rowControlsMap.value(oldTerm.value());
		_rowControlsValues[term.value()] = _rowControlsValues.value(oldTerm.value());
		_rowControlsMap.remove(oldTerm.value());
		_rowControlsValues.remove(oldTerm.value());
	}
	_replaceTerm(index, term);

	QModelIndex modelIndex = ListModelTermsAssigned::index(index, 0);
	emit dataChanged(modelIndex, modelIndex);
	emit keyTermChanged(oldTerm.value(), term.value()); // Change the key term for row component: this must be done after the data change is processed (the bound value must be first set)
}

