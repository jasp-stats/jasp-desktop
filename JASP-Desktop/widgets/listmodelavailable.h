#ifndef LISTMODELAVAILABLE_H
#define LISTMODELAVAILABLE_H

#include "listmodel.h"
#include "analysis/options/terms.h"
#include "analysis/options/variableinfo.h"

class ListModelAssigned;

class ListModelAvailable: public ListModel, public VariableInfoProvider
{
public:
	ListModelAvailable(AnalysisQMLForm *form, QQuickItem* item) : ListModel(form, item) {}
	
	virtual void termsAlreadyAssigned(const Terms &terms) = 0;
	virtual void addAssignedModel(ListModelAssigned* model) { assignedModels.push_back(model); }
	virtual void sendBack(Terms &terms) {}
	virtual const Terms& allTerms() const { return _allTerms; }

public slots:
	virtual void syncTermsChanged(Terms* termsAdded, Terms* termsRemoved) {}

protected:
	std::vector<ListModelAssigned*> assignedModels;
	std::vector<ListModel*> _syncModels;
	std::map<QString, ListModel*> _termSyncModelMap;
	
	Terms _allTerms;
};

#endif // LISTMODELAVAILABLE_H
