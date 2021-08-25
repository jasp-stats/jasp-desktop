#include "interactionmodel.h"
#include <QSet>

void InteractionModel::addFixedFactors(const Terms &terms, bool combineWithExistingTerms)
{
	_fixedFactors.add(terms);

	if (combineWithExistingTerms)
	{
		Terms existingTerms = _interactionTerms;
	
		Terms newTerms = _interactionTerms;
		newTerms.discardWhatDoesContainTheseComponents(_randomFactors);
		newTerms.discardWhatDoesContainTheseComponents(_covariates);
		existingTerms.add(newTerms.ffCombinations(terms));
		_interactionTerms.set(existingTerms);	
	}
	else
		_interactionTerms.add(terms);
}

void InteractionModel::addRandomFactors(const Terms &terms)
{
	_randomFactors.add(terms);
	_interactionTerms.add(terms);
}

void InteractionModel::addCovariates(const Terms &terms)
{
	_covariates.add(terms);
	_interactionTerms.add(terms);	
}

void InteractionModel::clearInteractions()
{
	_covariates.clear();
	_fixedFactors.clear();
	_randomFactors.clear();
	_interactionTerms.clear();
	_interactionTerms.removeParent();
}

void InteractionModel::removeInteractionTerms(const Terms& terms)
{
	_fixedFactors.remove(terms);
	_randomFactors.remove(terms);
	_covariates.remove(terms);
	if (_mustContainLowerTerms)
		_interactionTerms.discardWhatDoesContainTheseTerms(terms);
	else
		_interactionTerms.remove(terms);
}

QSet<int> InteractionModel::changeComponentName(const std::string& oldName, const std::string& newName)
{
	_fixedFactors.replaceVariableName(oldName, newName);
	_randomFactors.replaceVariableName(oldName, newName);
	_covariates.replaceVariableName(oldName, newName);

	return _interactionTerms.replaceVariableName(oldName, newName);
}
