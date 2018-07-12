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

#ifndef LISTMODELANOVAASSIGNED_H
#define LISTMODELANOVAASSIGNED_H

#include "listmodelassigned.h"
#include "listmodelavailable.h"
#include "options/optionterm.h"
#include "options/optionstable.h"

class ListModelAnovaAssigned : public ListModelAssigned
{
	Q_OBJECT
	
	enum AssignType { Cross = 0, MainEffects, Interaction, All2Way, All3Way, All4Way, All5Way };
	
public:
	explicit ListModelAnovaAssigned(AnalysisQMLForm *form, QQuickItem* item);
	
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;	
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;	
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	virtual void setSource(ListModelAvailable *source) OVERRIDE;
	
	virtual Terms *termsFromIndexes(const QList<int> &indexes) const OVERRIDE;
	virtual bool canDropTerms(const Terms *terms) const OVERRIDE;
	virtual bool dropTerms(const Terms *terms) OVERRIDE;
	virtual bool dropTerms(const Terms *terms, int assignType);
	virtual void removeTermsAfterBeingDropped(const QList<int> &indices) OVERRIDE;

	const Terms &terms() const OVERRIDE;
	
public slots:
	virtual void availableTermsChanged(Terms* termsToAdd, Terms* termsToRemove) OVERRIDE;
	
protected:
	void addFixedFactors(const Terms &terms);
	void addRandomFactors(const Terms &terms);
	void addCovariates(const Terms &terms);
	void removeVariables(const Terms &terms);
	
	QString getItemType(const Term &term);
	
	static OptionTerm* termOptionFromRow(Options *row);
	
	void setTerms(const Terms &terms, bool newTermsAreNuisance = false);
	
	void clear();
	void assign(const Terms &terms);
	void updateNuisances(bool checked = true);
	
	OptionsTable *_boundTo;
	
	std::vector<Options *> _rows;
	
	bool _piecesCanBeAssigned;
	
	Terms _covariates;
	Terms _fixedFactors;
	Terms _randomFactors;
	
	Terms _terms;	
};


#endif // LISTMODELANOVAASSIGNED_H
