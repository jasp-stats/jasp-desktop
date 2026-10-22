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
			///< (ORIGIN-AUTHORITATIVE): the universe was rebuilt from this pool's
			///  provider. `filterIdToUpdate` is the PARTICIPATION KEY - the filterId the affected
			///  terms currently carry (the old selection id on a host switch, the unchanged
			///  selection id on a data change, -1 for a hostless/default-provider pool); there is
			///  deliberately NO "new id" parameter: the rebuilt universe terms are delivered
			///  birth-stamped (FIX 1) and the replacements ride THEM - a single new id would
			///  presume one uniform new namespace, which a derived pool with mixed sources has
			///  not. The assigned models reconcile their references with it: a term stamped
			///  `filterIdToUpdate` is a reference into the provider that changed - a surviving
			///  name is REPLACED BY the delivered universe term, a vanished name is removed;
			///  foreign stamps and -1 terms under a qualified provider are other providers'
			///  references and stay. Emitted on every provider-driven rebuild (also when only
			///  the selection moved, no column changed).
			void availableTermsReset(Terms termsAdded, Terms termsRemoved, int filterIdToUpdate);

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
	///< the PARTICIPATION KEY of this pool - the filterId the terms of the last
	///< built universe carry (the pool's own selection at that rebuild, else its source chain,
	///< else -1 = the default provider). It is the from half of the transition the next rebuild
	///< reports through availableTermsReset AND the participation key of the assigned prune
	///< (removeTermsInAssignedList); updated only AFTER a rebuild's prune has run, so the prune
	///< always sees the provider the affected references point into.
	int									_universeFilterId	= -1;

	QList<ListModelAssignedInterface*>	_assignedModels;
};

#endif // LISTMODELTERMSAVAILABLE_H
