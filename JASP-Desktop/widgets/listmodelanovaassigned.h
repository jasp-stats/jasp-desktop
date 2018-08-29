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

#include "listmodeltermsassignedinterface.h"
#include "listmodeltermsavailableinterface.h"
#include "analysis/options/options.h"
#include "analysis/options/optionterm.h"

class ListModelAnovaAssigned : public ListModelTermsAssignedInterface
{
	Q_OBJECT
	
	enum AssignType { Cross = 0, MainEffects, Interaction, All2Way, All3Way, All4Way, All5Way };
	
public:
	explicit ListModelAnovaAssigned(AnalysisQMLForm *form, QQuickItem* item);

	void initTerms(const std::vector<Options*> &terms, Options* rowTemplate);
	
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;	
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;	
	virtual void setSource(ListModelTermsAvailableInterface *source) OVERRIDE;
	
	virtual Terms *termsFromIndexes(const QList<int> &indexes) const OVERRIDE;
	virtual bool canAddTerms(Terms *terms) const OVERRIDE;
	virtual Terms* addTerms(Terms *terms, int dropItemIndex = -1) OVERRIDE;
	virtual void removeTerms(const QList<int> &indices) OVERRIDE;
	virtual const Terms &terms() const OVERRIDE;
	
	const std::vector<Options *> &rows() const;
	
public slots:
	virtual void availableTermsChanged(Terms* termsToAdd, Terms* termsToRemove) OVERRIDE;
	
protected:
	Terms* _addTerms(Terms *terms, int assignType);
	void addFixedFactors(const Terms &terms);
	void addRandomFactors(const Terms &terms);
	void addCovariates(const Terms &terms);
	void removeVariables(const Terms &terms);
	
	QString getItemType(const Term &term);
	
	static OptionTerm* termOptionFromRow(Options *row);
	
	void setTerms(const Terms &terms, bool newTermsAreNuisance = false);
	
	void updateNuisances(bool checked = true);
	
	std::vector<Options *> _rows;
	Options* _rowTemplate;
	
	bool _piecesCanBeAssigned;
	
	Terms _covariates;
	Terms _fixedFactors;
	Terms _randomFactors;
	
	Terms _anovaTerms;	
};


#endif // LISTMODELANOVAASSIGNED_H
