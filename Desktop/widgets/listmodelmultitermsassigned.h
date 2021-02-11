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
	
	void			initTerms(const Terms &terms, const RowControlsValues& allValuesMap = RowControlsValues())		override;
	Terms			addTerms(const Terms &terms, int dropItemIndex = -1, JASPControl::AssignType assignOption = JASPControl::AssignType::AssignDefault)	override;
	void			moveTerms(const QList<int>& indexes, int dropItemIndex = -1)										override;
	void			removeTerms(const QList<int> &indexes)																override;

	const QList<Terms>&	tuples() const { return _tuples; }
private:
	void			_setTerms();

protected:
	int				_columns = 2;
	QList<Terms>	_tuples;
	bool			_allowDuplicatesInMultipleColumns = false;
};

#endif // LISTMODELMULTITERMSASSIGNED_H
