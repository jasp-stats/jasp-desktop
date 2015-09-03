#ifndef REGRESSIONLOGLINEARBAYESIANFORM_H
#define REGRESSIONLOGLINEARBAYESIANFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class RegressionLogLinearBayesianForm;
}

class RegressionLogLinearBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit RegressionLogLinearBayesianForm(QWidget *parent = 0);
	~RegressionLogLinearBayesianForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void factorsChanging();
	void factorsChanged();
	
private:
	Ui::RegressionLogLinearBayesianForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_factorsListModel;

	TableModelAnovaModel *_model;

	TableModelVariablesAvailable *_factorsAvailableListModel;
};

#endif // REGRESSIONLOGLINEARBAYESIANFORM_H
