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

void ListModelTermsAssigned::initTerms(const Terms &terms, const RowControlsValues& allValuesMap, bool reInit)
{
	ListModelAssignedInterface::initTerms(terms, allValuesMap, reInit);

	if (availableModel() != nullptr)
	{
		if (!_copyTermsWhenDropped)
			availableModel()->removeTermsInAssignedList();
	}
}

void ListModelTermsAssigned::availableTermsResetHandler(Terms termsAdded, Terms termsRemoved)
{
	if (termsAdded.size() > 0 && listView()->addAvailableVariablesToAssigned())
	{
		beginResetModel();
		_addTerms(termsAdded);
		endResetModel();

		if (!_copyTermsWhenDropped)
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

Terms ListModelTermsAssigned::addTerms(const Terms& termsToAdd, int dropItemIndex, const RowControlsValues& rowValues)
{
	Terms termsToSendBack;
	int maxRows = listView()->maxRows(); // maxRows == -1 means no maximum

	if (termsToAdd.size() == 0)
		return termsToSendBack;
	else if (maxRows > 0 && dropItemIndex >= maxRows)
		return termsToAdd;

	Terms newTerms;

	if (dropItemIndex < 0 && maxRows == 1)
		dropItemIndex = 0; // for single row, per default replace old item by new one.

	for (const auto& it : rowValues.toStdMap())
		_rowControlsValues[it.first] = it.second;

	if (dropItemIndex == 0 && maxRows == termsToAdd.size())
	{
		// If we replace all the items, use beginResetModel
		termsToSendBack = terms();
		newTerms = termsToAdd;

		beginResetModel();
		_setTerms(newTerms);
		endResetModel();
	}
	else
	{
		// We try to use beginInsertRows/endInsetRows (and beginRemoveRows/endRemoveRows) to set the values instead of beginResetModel: this is indeed the right way to use QAbstractItemModel
		// By using beginResetModel/endResetModel all QML objects of the list are removed and rebuild again. This should not be a problem, apart from one special case:
		// in a TabView, if the user changes the title of a Tab and clicks direclty the '+' button to add another tab, adding a new tab will be done first, and will add a new
		// term to the model: if the model of the TabView is reset, the TextField controls that handle the titles of the Tabs are destroyed and recreated. As the TextField control
		// that was used to change the title is destroyed, the signal that changes this title is not received, and the title gets back its old value.
		newTerms = terms();
		if (dropItemIndex >= 0 && dropItemIndex < terms().size())
			newTerms.insert(dropItemIndex, termsToAdd);
		else
		{
			dropItemIndex = terms().size();
			newTerms.add(termsToAdd);
		}

		beginInsertRows(QModelIndex(), dropItemIndex, dropItemIndex + termsToAdd.size() - 1);
		_setTerms(newTerms);
		endInsertRows();

		if (maxRows > 0 && newTerms.size() > maxRows)
		{
			for (size_t i = maxRows; i < newTerms.size(); i++)
				termsToSendBack.add(newTerms.at(i));
			newTerms.remove(maxRows, newTerms.size() - maxRows);

			beginRemoveRows(QModelIndex(), maxRows, maxRows + termsToSendBack.size());
			_setTerms(newTerms);
			endRemoveRows();
		}
	}

	return termsToSendBack;
}

void ListModelTermsAssigned::removeTerm(int index)
{
	if (index < 0 || index >= rowCount()) return;

	beginResetModel();

	const Term& term = terms().at(size_t(index));
	const QString& termQ = term.asQString();

	RowControls* controls = _rowControlsMap.value(termQ);
	if (controls)
	{
		for (JASPControl* control : controls->getJASPControlsMap().values())
		{
			control->setHasError(false);
			listView()->form()->clearControlError(control);
		}

		_rowControlsMap.remove(termQ);
	}
	_removeTerm(term);

	endResetModel();
}

void ListModelTermsAssigned::changeTerm(int index, const QString& name)
{
	QString oldName = terms()[size_t(index)].asQString();
	if (oldName != name)
	{
		_rowControlsMap[name] = _rowControlsMap.value(oldName);
		_rowControlsValues[name] = _rowControlsValues.value(oldName);
		_rowControlsMap.remove(oldName);
		_rowControlsValues.remove(oldName);
		_replaceTerm(index, Term(name));

		emit oneTermChanged(oldName, name);
		QModelIndex modelIndex = ListModelTermsAssigned::index(index, 0);
		emit dataChanged(modelIndex, modelIndex);
	}
}

