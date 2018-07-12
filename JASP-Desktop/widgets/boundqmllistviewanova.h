#ifndef BOUNDQMLLISTVIEWANOVA_H
#define BOUNDQMLLISTVIEWANOVA_H

#include "boundqmllistview.h"
#include "options/optionstable.h"
#include "listmodelanovaassigned.h"

class BoundQMLListViewAnova : public BoundQMLListView
{
	Q_OBJECT
	
public:
	explicit BoundQMLListViewAnova(QQuickItem* item, AnalysisQMLForm* form);
	
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	virtual void setUp() OVERRIDE;
	virtual Option* createOption() OVERRIDE;
		
private:
	OptionsTable* _boundTo;
	
};

#endif // BOUNDQMLLISTVIEWANOVA_H
