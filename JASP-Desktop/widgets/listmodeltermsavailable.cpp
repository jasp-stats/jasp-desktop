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

#include "listmodeltermsavailable.h"
#include "listmodeltermsassigned.h"
#include "analysisforms/analysisqmlform.h"
#include <QQmlProperty>

ListModelTermsAvailable::ListModelTermsAvailable(AnalysisQMLForm *form, QQuickItem* item)
	: ListModelAvailable(form, item)
{
}

void ListModelTermsAvailable::setUp()
{
	ListModelAvailable::setUp();
	QStringList syncModels = QQmlProperty(_item, "syncModels").read().toStringList();
	if (syncModels.isEmpty())
	{
		addError(QString::fromLatin1("Needs sync model for Variables ListView ") + _name);
	}
	else
	{
		for (const QString& syncModel : syncModels)
		{
			if (syncModel != "_JASPAllVariables")
			{
				ListModel* model = _form->getModel(syncModel);
				if (model)
				{
					_syncModels.push_back(model);
					connect(model, &ListModel::termsChanged, this, &ListModelTermsAvailable::syncTermsChanged);
				}
				else
					addError(QString::fromLatin1("Unknown sync model ") + syncModel + QString::fromLatin1(" for Variables ListView ") + _name);
			}
		}
		resetTermsFromSyncModels();
	}
}

void ListModelTermsAvailable::initTerms(const Terms &terms)
{	
	beginResetModel();

	Terms suggested;
	Terms allowed;
	Terms forbidden;

	foreach (const Term &term, terms)
	{
		if ( ! isAllowed(term))
			forbidden.add(term);
		else if (isSuggested(term))
			suggested.add(term);
		else
			allowed.add(term);
	}
	Terms ordered; // present them in a nice order

	ordered.add(suggested);
	ordered.add(allowed);
	ordered.add(forbidden);

	_allTerms.set(ordered);
	_terms.removeParent();
	_terms.set(ordered);

	_terms.setSortParent(_allTerms);

	endResetModel();
}

QVariant ListModelTermsAvailable::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	return VariableInfoConsumer::requestInfo(term, info);
}

void ListModelTermsAvailable::termsAlreadyAssigned(const Terms &terms)
{
	if (_removeTermsWhenDropped && terms.size() > 0)
	{
		beginResetModel();
		_terms.remove(terms);
		endResetModel();
	}
}

void ListModelTermsAvailable::sendBack(Terms &terms)
{
	if (_removeTermsWhenDropped)
	{
		beginResetModel();
		_terms.add(terms);
		endResetModel();
	}
}

void ListModelTermsAvailable::syncTermsChanged(Terms* termsAdded, Terms* termsRemoved)
{
	Q_UNUSED(termsAdded);
	Q_UNUSED(termsRemoved);
	
	Options* options = _form->getAnalysisOptions();
	if (options != NULL)
		options->blockSignals(true);
	
	resetTermsFromSyncModels();
	emit termsChanged(&_tempAddedTerms, &_tempRemovedTerms);
	
	if (options != NULL)
		options->blockSignals(false);	
}

void ListModelTermsAvailable::_setChangedTerms(const Terms &newTerms)
{
	_tempRemovedTerms.clear();
	_tempAddedTerms.clear();
	for (const Term& term : _allTerms)
	{
		if (!newTerms.contains(term))
			_tempRemovedTerms.add(term);
	}
	
	for (const Term& term : newTerms)
	{
		if (!_allTerms.contains(term))
			_tempAddedTerms.add(term);
	}
}

void ListModelTermsAvailable::resetTermsFromSyncModels()
{
	Terms termsAvailable;
	_termSyncModelMap.empty();
	for (ListModel* syncModel : _syncModels)
	{
		const Terms& terms = syncModel->terms();
		for (const Term& term : terms)
			_termSyncModelMap[term.asQString()] = syncModel;
		termsAvailable.add(terms);
	}
	
	_setChangedTerms(termsAvailable);
	initTerms(termsAvailable);
	
	for (ListModelAssigned* modelAssign : assignedModels)
	{
		ListModel* model = dynamic_cast<ListModel*>(modelAssign);
		if (model)
			termsAlreadyAssigned(model->terms());
	}
}

ListModel *ListModelTermsAvailable::getSyncModelOfTerm(const Term &term)
{
	return _termSyncModelMap[term.asQString()];
}
