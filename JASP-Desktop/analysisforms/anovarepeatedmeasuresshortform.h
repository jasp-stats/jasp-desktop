#ifndef ANOVAREPEATEDMEASURESSHORTFORM_H
#define ANOVAREPEATEDMEASURESSHORTFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"
#include "widgets/tablemodelanovadesign.h"
#include "widgets/tablemodelanovawithinsubjectcells.h"

namespace Ui {
class AnovaRepeatedMeasuresShortForm;
}

class AnovaRepeatedMeasuresShortForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaRepeatedMeasuresShortForm(QWidget *parent = 0);
	~AnovaRepeatedMeasuresShortForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	
private slots:
	void factorsChanged();
	void termsChanged();
	void withinSubjectsDesignChanged();

	void anovaDesignTableClicked(QModelIndex index);

private:
	Ui::AnovaRepeatedMeasuresShortForm *ui;

	TableModelAnovaDesign *_designTableModel;
	TableModelAnovaWithinSubjectCells *_withinSubjectCellsListModel;
	TableModelVariablesAssigned *_betweenSubjectsFactorsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAREPEATEDMEASURESSHORTFORM_H
