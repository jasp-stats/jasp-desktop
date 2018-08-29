#ifndef BOUNDQMLLISTVIEWANOVA_H
#define BOUNDQMLLISTVIEWANOVA_H

#include "boundqmldraggablelistview.h"
#include "analysis/options/optionstable.h"
#include "listmodelanovaassigned.h"

class BoundQMLListViewAnova : public BoundQMLDraggableListView
{
	Q_OBJECT
	
public:
	explicit BoundQMLListViewAnova(QQuickItem* item, AnalysisQMLForm* form);
	
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	virtual Option* createOption() OVERRIDE;

private slots:
	void modelChangedHandler();

private:
	OptionsTable* _boundTo;
	ListModelAnovaAssigned* _anovaModel;
	
};

#endif // BOUNDQMLLISTVIEWANOVA_H
