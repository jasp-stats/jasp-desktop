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

#include "listmodelavailableinterface.h"
#include "listmodelassignedinterface.h"
#include "qmllistviewtermsavailable.h"
#include "log.h"

void ListModelAvailableInterface::initTerms(const Terms &terms, const RowControlsOptions&)
{
	beginResetModel();
	
	_allTerms = _allSortedTerms = _terms = terms;
	_terms.setSortParent(_allSortedTerms);

	if (currentSortType() != SortType::None)
		Sortable::sortItems();

	if (_addEmptyValue && _allSortedTerms.size() > 0 && !_allSortedTerms[0].asQString().isEmpty())
	{
		_allSortedTerms.insert(0, QString());
		_allTerms.insert(0, QString());
		_terms.add(QString());
	}

	removeTermsInAssignedList();
	endResetModel();
}

QVariant ListModelAvailableInterface::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	return VariableInfoConsumer::requestInfo(term, info);
}

void ListModelAvailableInterface::sortItems(SortType sortType)
{
	beginResetModel();

	switch(sortType)
	{
	case SortType::None:
		_allSortedTerms = _allTerms;
		break;

	case SortType::SortByName:
	{
		QList<QString> sortedTerms = _allSortedTerms.asQList();
		std::sort(sortedTerms.begin(), sortedTerms.end(),
				  [&](const QString& a, const QString& b) {
						return a.compare(b, Qt::CaseInsensitive) < 0;
					});
		_allSortedTerms = Terms(sortedTerms);
		break;
	}

	case SortType::SortByType:
	{
		QList<QString>				termsList = _allSortedTerms.asQList();
		QList<QPair<QString, int> > termsTypeList;

		for (const QString& term : termsList)
			termsTypeList.push_back(QPair<QString, int>(term, requestInfo(term, VariableInfo::VariableType).toInt()));

		std::sort(termsTypeList.begin(), termsTypeList.end(),
				  [&](const QPair<QString, int>& a, const QPair<QString, int>& b) {
						return a.second - b.second > 0;
					});

		QList<QString> sortedTerms;

		for (const auto& term : termsTypeList)
			sortedTerms.push_back(term.first);

		_allSortedTerms = Terms(sortedTerms);
		break;
	}

	default:
		Log::log() << "Unimplemented sort in ListModelAvailableInterface::sortItems!";
		break;
	}

	Terms orgTerms = _terms;
	_terms.clear();
	_terms.set(orgTerms); // This will reorder the terms

	endResetModel();
}

void ListModelAvailableInterface::sourceTermsChanged(const Terms* termsAdded, const Terms* termsRemoved)
{
	Q_UNUSED(termsAdded);
	Q_UNUSED(termsRemoved);
	
	resetTermsFromSourceModels();
}

void ListModelAvailableInterface::setChangedTerms(const Terms &newTerms)
{
	_tempRemovedTerms.clear();
	_tempAddedTerms.clear();

	for (const Term& term : _allTerms)
		if (!newTerms.contains(term))
			_tempRemovedTerms.add(term);

	for (const Term& term : newTerms)
		if (!_allTerms.contains(term))
			_tempAddedTerms.add(term);
}

void ListModelAvailableInterface::removeTermsInAssignedList()
{
	beginResetModel();
	
	_terms = _allSortedTerms;
	_terms.setSortParent(_allSortedTerms);
	
	QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(listView());

	if (qmlAvailableListView)
		for (ListModelAssignedInterface* modelAssign : qmlAvailableListView->assignedModel())
		{
			Terms assignedTerms = modelAssign->terms();
			if (assignedTerms.discardWhatIsntTheseTerms(_allSortedTerms))
			{
				modelAssign->initTerms(assignedTerms); // initTerms call removeTermsInAssignedList
				emit modelAssign->modelChanged();
			}
			else if (!modelAssign->copyTermsWhenDropped())
				_terms.remove(assignedTerms);
		}

	
	endResetModel();
}

