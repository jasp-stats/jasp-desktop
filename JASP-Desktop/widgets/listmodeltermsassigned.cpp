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
#include <QTimer>
#include <QDebug>

using namespace std;

ListModelTermsAssigned::ListModelTermsAssigned(AnalysisQMLForm *form, QQuickItem* item)
	: ListModelAssigned(form, item)
{
	_boundTo = NULL;
	_source = NULL;
}

void ListModelTermsAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionTerms *>(option);

	if (_boundTo != NULL)
	{
		if (_source != NULL)
		{
			const vector<vector<string> > assigned = _boundTo->value();

			beginResetModel();
			_terms.set(assigned);
			endResetModel();

			_source->termsAlreadyAssigned(_terms);
		}
		else
		{
			qDebug() << "TableModelTermsAssigned::bindTo(); source not set";
		}
	}
	else
	{
		qDebug() << "TableModelTermsAssigned::bindTo(); option not of type OptionVariables*";
	}
}

void ListModelTermsAssigned::unbind()
{
	_boundTo = NULL;
}

void ListModelTermsAssigned::availableTermsChanged(Terms* termsAdded, Terms* termsRemoved)
{
	// Only remove the terms that are not available anymore
	Q_UNUSED(termsAdded);
	
	if (termsRemoved && termsRemoved->size() > 0)
	{
		unassign(*termsRemoved);
		_tempTermsToRemove.set(termsRemoved);
		emit termsChanged(NULL, &_tempTermsToRemove);
	}	
}

bool ListModelTermsAssigned::canDropTerms(const Terms *terms) const
{
	if (_boundTo == NULL)
		return false;

	if ( ! ListModel::canDropTerms(terms))
		return false;

	if (_boundTo->onlyOneTerm())
	{
		if (terms->size() != 1)
			return false;
	}

	return true;
}

bool ListModelTermsAssigned::dropTerms(const Terms *terms)
{
	if (_boundTo == NULL)
		return false;

	if ( ! canDropTerms(terms))
		return false;

	_delayDropped.set(terms->terms());
	QTimer::singleShot(0, this, SLOT(delayAssignDroppedData()));

	return true;
}

void ListModelTermsAssigned::delayAssignDroppedData()
{
	assign(_delayDropped);
	emit termsChanged(&_delayDropped, NULL);
}

void ListModelTermsAssigned::removeTermsAfterBeingDropped(const QList<int> &indices)
{

	_tempTermsToRemove.clear();
	for (const int &index : indices)
		_tempTermsToRemove.add(_terms.at(index));
	
	unassign(_tempTermsToRemove);

	emit termsChanged(NULL, &_tempTermsToRemove);	
}


void ListModelTermsAssigned::assign(const Terms &terms)
{
	if (_boundTo == NULL)
		return;

	Terms newTerms;

	if (_boundTo->onlyOneTerm())
	{
		if (terms.size() > 0)
			newTerms.add(terms.at(0));

		if (_terms.size() > 0)
		{
			_toSendBack.set(_terms);
			_terms.clear();
			QTimer::singleShot(0, this, SLOT(sendBackToSource()));
		}
	}
	else
	{
		newTerms.set(_terms);
		newTerms.add(terms);
	}

	setAssigned(newTerms);
}

void ListModelTermsAssigned::unassign(const Terms &terms)
{
	Terms termsToKeep;
	termsToKeep.set(_terms);
	termsToKeep.remove(terms);
	setAssigned(termsToKeep);
}

void ListModelTermsAssigned::setAssigned(const Terms &terms)
{
	if (_source == NULL)
	{
		qDebug() << "TableModelVariablesAssigned::setAssigned() : Source not set!";
		return;
	}

	beginResetModel();
	_terms.set(terms);
	endResetModel();

	if (_boundTo != NULL)
		_boundTo->setValue(_terms.asVectorOfVectors());
}

void ListModelTermsAssigned::sendBackToSource()
{
	_source->sendBack(_toSendBack);
	_toSendBack.clear();
}

