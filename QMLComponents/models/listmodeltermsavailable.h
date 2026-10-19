//
// Copyright (C) 2013-2025 University of Amsterdam
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

#ifndef LISTMODELTERMSAVAILABLE_H
#define LISTMODELTERMSAVAILABLE_H

#include "listmodeldraggable.h"
#include "terms.h"
#include "sortmenumodel.h"
#include "sortable.h"

class ListModelAssignedInterface;

class ListModelTermsAvailable: public ListModelDraggable, public Sortable
{
	Q_OBJECT
public:
	ListModelTermsAvailable(JASPListControl* listView, const Terms& terms = Terms());

	virtual const Terms& allTerms()																											const { return _allSortedTerms; }
    void initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& _rowControlsValues = {})                          override;
	virtual void removeTermsInAssignedList();
	
			void sortItems(SortType sortType)											override;
			Terms addTerms(const Terms& terms, int dropItemIndex = -1, const Terms::RelatedValuesPerTerm& rowValues = {})	override;

			void										addAssignedModel(ListModelAssignedInterface* model);
			const QList<ListModelAssignedInterface*>&	assignedModels()	const			{ return _assignedModels; }

signals:
			void availableTermsReset(Terms termsAdded, Terms termsRemoved);

public slots:
			void sourceTermsReset()															override;
			void sourceVariableNamesChanged(QMap<QString, QString> map)						override;
			void sourceVariablesChanged(QStringList columns)								override;
			bool sourceVariableTypeChanged(QString columnName, columnType colType)						override;
			bool sourceLabelsChanged(QString columnName, QMap<QString, QString> = {})		override;
			bool sourceLabelsReordered(QString columnName)									override;
			void removeAssignedModel(ListModelAssignedInterface *assignedModel);
			void clearAssignedModels() { _assignedModels.clear(); }

protected:
	Terms								_allTerms;
	Terms								_allSortedTerms;

	QList<ListModelAssignedInterface*>	_assignedModels;
};

#endif // LISTMODELTERMSAVAILABLE_H
