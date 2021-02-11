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

#ifndef LISTMODELAVAILABLEINTERFACE_H
#define LISTMODELAVAILABLEINTERFACE_H

#include "listmodeldraggable.h"
#include "analysis/terms.h"
#include "analysis/variableinfo.h"
#include "sortmenumodel.h"
#include "sortable.h"

class ListModelAssignedInterface;

class ListModelAvailableInterface: public ListModelDraggable, public VariableInfoProvider, public Sortable
{
	Q_OBJECT
public:
	ListModelAvailableInterface(JASPListControl* listView)
		: ListModelDraggable(listView) {}
	
	virtual const Terms& allTerms()																						const { return _allSortedTerms; }
			void initTerms(const Terms &terms, const RowControlsValues& _rowControlsValues = RowControlsValues())	override;
	virtual void resetTermsFromSources(bool updateAssigned = true)			= 0;
	virtual void removeTermsInAssignedList();
	
			QVariant requestInfo(const Term &term, VariableInfo::InfoType info)			const override;

			void sortItems(SortType sortType)											override;

			void										addAssignedModel(ListModelAssignedInterface* model);
			const QList<ListModelAssignedInterface*>&	assignedModel()	const			{ return _assignedModels; }

signals:
			void availableTermsReset(Terms termsAdded, Terms termsRemoved);

public slots:
			void sourceTermsReset()															override;
			void sourceNamesChanged(QMap<QString, QString> map)								override;
			void sourceColumnsChanged(QStringList columns)									override;
			int  sourceColumnTypeChanged(QString name)										override;
			bool sourceLabelsChanged(QString columnName, QMap<QString, QString> = {})		override;
			bool sourceLabelsReordered(QString columnName)									override;
			void removeAssignedModel(ListModelDraggable* model);

protected:
	Terms								_allTerms;
	Terms								_allSortedTerms;

	QList<ListModelAssignedInterface*>	_assignedModels;
};

#endif // LISTMODELTERMSAVAILABLEINTERFACE_H
