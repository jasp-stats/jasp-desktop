#ifndef ANOVABAYESIANFORM_H
#define ANOVABAYESIANFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class AnovaBayesianForm;
}

class AnovaBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaBayesianForm(QWidget *parent = 0);
	~AnovaBayesianForm();

private slots:
	void factorsChanged();
	
private:
	Ui::AnovaBayesianForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;
};

#endif // ANOVABAYESIANFORM_H
