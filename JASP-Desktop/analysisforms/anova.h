#ifndef ANOVA_H
#define ANOVA_H

#include "analysisform.h"

#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodel.h"

namespace Ui {
class ANOVA;
}

class Anova : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit Anova(QWidget *parent = 0);
	~Anova();

	virtual void set(Options *options, DataSet *dataSet) override;
	
private slots:
	void factorsChanged();
	void dependentChanged();

private:
	Ui::ANOVA *ui;

	ListModelVariablesAssigned *_dependentListModel;
	ListModelVariablesAssigned *_fixedFactorsListModel;
	ListModelVariablesAssigned *_randomFactorsListModel;
	ListModelVariablesAssigned *_wlsWeightsListModel;

	ListModelAnovaModel *_anovaModel;

	ListModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVA_H
