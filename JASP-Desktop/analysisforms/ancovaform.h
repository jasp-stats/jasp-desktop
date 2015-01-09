#ifndef ANCOVAFORM_H
#define ANCOVAFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"

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
	void termsChanged();

private:
	Ui::AncovaForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

    TableModelVariablesAvailable *_plotFactorsAvailableTableModel;
    TableModelVariablesAssigned *_horizontalAxisTableModel;
    TableModelVariablesAssigned *_seperateLinesTableModel;
    TableModelVariablesAssigned *_seperatePlotsTableModel;

};

#endif // ANCOVAFORM_H
