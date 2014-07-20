#ifndef ANOVAREPEATEDMEASURESSHORTFORM_H
#define ANOVAREPEATEDMEASURESSHORTFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"
#include "widgets/tablemodelanovadesign.h"

namespace Ui {
class AnovaRepeatedMeasuresShortForm;
}

class AnovaRepeatedMeasuresShortForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaRepeatedMeasuresShortForm(QWidget *parent = 0);
	~AnovaRepeatedMeasuresShortForm();
	
private slots:
	void factorsChanged();
	void termsChanged();

private:
	Ui::AnovaRepeatedMeasuresShortForm *ui;

	TableModelAnovaDesign *_designTableModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAREPEATEDMEASURESSHORTFORM_H
