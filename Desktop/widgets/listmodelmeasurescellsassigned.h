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

#ifndef LISTMODELMEASURESCELLSASSIGNED_H
#define LISTMODELMEASURESCELLSASSIGNED_H

#include "listmodelassignedinterface.h"

class ListModelRepeatedMeasuresFactors;

class ListModelMeasuresCellsAssigned : public ListModelAssignedInterface
{
	Q_OBJECT
public:
	ListModelMeasuresCellsAssigned(JASPListControl* listView);

	int				rowCount(const QModelIndex &parent = QModelIndex())												const	override { return _levels.size() * 2; }
	QVariant		data(const QModelIndex &index, int role = Qt::DisplayRole)										const	override;
	Terms			termsFromIndexes(const QList<int> &indexes)														const	override;
	void			initTerms(const Terms &terms, const RowControlsOptions& allOptionsMap = RowControlsOptions())			override;
	Terms			addTerms(const Terms& terms, int dropItemIndex = -1, JASPControl::AssignType assignOption = JASPControl::AssignType::AssignDefault)	override;
	void			moveTerms(const QList<int>& indexes, int dropItemIndex = -1)											override;
	void			removeTerms(const QList<int>& indexes) override;

	void			initLevels(const Terms& levels, const Terms &variables = Terms(), bool initVariables = false);

public slots:	
	void			sourceTermsChanged(const Terms* termsAdded, const Terms* termsRemoved) override;
	
private:
	void			_fitTermsWithLevels();

	QList<QString> _levels;
};

#endif // LISTMODELMEASURESCELLSASSIGNED_H
