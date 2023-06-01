#ifndef INTERACTIONMODEL_H
#define INTERACTIONMODEL_H

#include "terms.h"

class InteractionModel
{
public:
	InteractionModel() {}
	
	void addInteractionTerms(const Terms &terms) { _interactionTerms.add(terms); }
	void addFixedFactors(const Terms &terms, bool combineWithExistingTerms = true);
	void addRandomFactors(const Terms &terms);
	void addCovariates(const Terms &terms);
	void removeInteractionTerms(const Terms& terms);
	void clearInteractions();
	QSet<int> changeComponentName(const std::string& oldName, const std::string& newName);

	const Terms& interactionTerms() const { return _interactionTerms; }

protected:	
	Terms _covariates;
	Terms _fixedFactors;
	Terms _randomFactors;
	
	Terms _interactionTerms;
	bool  _mustContainLowerTerms = true;
};

#endif // INTERACTIONMODEL_H
