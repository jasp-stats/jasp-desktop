#ifndef ANOVABAYESIANFORM_H
#define ANOVABAYESIANFORM_H

#include "analysisform.h"

#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodelnuisancefactors.h"

namespace Ui {
class AnovaBayesianForm;
}

class AnovaBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaBayesianForm(QWidget *parent = 0);
	~AnovaBayesianForm();

	virtual void set(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void factorsChanged();
	void dependentChanged();
	
private:
	Ui::AnovaBayesianForm *ui;

	ListModelVariablesAssigned *_dependentListModel;
	ListModelVariablesAssigned *_fixedFactorsListModel;
	ListModelVariablesAssigned *_randomFactorsListModel;
	ListModelVariablesAssigned *_wlsWeightsListModel;

	ListModelAnovaModelNuisanceFactors *_anovaModel;

	ListModelVariablesAvailable *_factorsAvailableListModel;
};

#endif // ANOVABAYESIANFORM_H
