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

#ifndef LISTMODELMULTITERMSASSIGNED_H
#define LISTMODELMULTITERMSASSIGNED_H

#include "listmodelassignedinterface.h"

class ListModelMultiTermsAssigned: public ListModelAssignedInterface
{
	Q_OBJECT
public:
	ListModelMultiTermsAssigned(JASPListControl* listView, int columns = 2);

    void			initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap = {})                         		override;
	Terms			canAddTerms(const Terms& terms)																				const	override;
	Terms			addTerms(const Terms &terms, int dropItemIndex = -1, const Terms::RelatedValuesPerTerm& rowValues = {})				override;
	void			moveTerms(const QList<int>& indexes, int dropItemIndex = -1)														override;
	void			removeTerms(const QList<int> &indexes)																				override;

	const QList<Terms>&	tuples() const { return _tuples; }

public slots:
	void			availableTermsResetHandler(Terms termsToAdd, Terms termsToRemove, int filterIdToUpdate)				override;
	void			sourceVariableNamesChanged(QMap<QString, QString> map)										override;


private:
	void			_setTerms();
	///< mds-plan post (ORIGIN-AUTHORITATIVE): the tuples are the family's source of truth, so the
	///  reconcile runs over them (the base class version would edit the flattened _terms only,
	///  which the next _setTerms() would revert) - survivors are REPLACED BY the delivered
	///  universe term, vanished cells are collected for the cell-clearing machinery; the row
	///  maps re-key rename-flow style.
	Terms			reconcileProviderChange(int filterIdToUpdate, const Terms &newUniverse)	override;

protected:
	int				_columns = 2;
	QList<Terms>	_tuples;
	bool			_allowDuplicatesInMultipleColumns = false;
};

#endif // LISTMODELMULTITERMSASSIGNED_H
