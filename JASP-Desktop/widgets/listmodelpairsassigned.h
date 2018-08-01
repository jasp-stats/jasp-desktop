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

#ifndef LISTMODELPAIRSASSIGNED_H
#define LISTMODELPAIRSASSIGNED_H

#include "listmodelassigned.h"
#include "analysis/options/optionvariablesgroups.h"

class ListModelPairsAssigned: public ListModelAssigned 
{
public:
	explicit ListModelPairsAssigned(AnalysisQMLForm *form, QQuickItem* item);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;

	virtual void setSource(ListModelAvailable *source) OVERRIDE;
	
	virtual Terms *termsFromIndexes(const QList<int> &indexes) const OVERRIDE;
	virtual const Terms& terms() const OVERRIDE;
	virtual bool canDropTerms(const Terms *terms) const OVERRIDE;
	virtual bool dropTerms(const Terms *terms) OVERRIDE;
	virtual void removeTermsAfterBeingDropped(const QList<int> &indexes) OVERRIDE;

protected:
	void assignToOption();

private:
	OptionVariablesGroups *_boundTo;
	Terms _values;

};

#endif // LISTMODELPAIRSASSIGNED_H
