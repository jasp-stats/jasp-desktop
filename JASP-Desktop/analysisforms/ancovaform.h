#ifndef ANCOVAFORM_H
#define ANCOVAFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class AncovaForm;
}

class AncovaForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AncovaForm(QWidget *parent = 0);
	~AncovaForm();
	
private slots:
	void factorsChanged();
	void dependentChanged();

private:
	Ui::AncovaForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANCOVAFORM_H
