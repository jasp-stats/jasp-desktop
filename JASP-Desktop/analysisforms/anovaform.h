#ifndef ANOVAFORM_H
#define ANOVAFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"

namespace Ui {
class AnovaForm;
}

class AnovaForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaForm(QWidget *parent = 0);
	~AnovaForm();
	
private slots:
	void factorsChanged();
	void termsChanged();

private:
	Ui::AnovaForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAFORM_H
