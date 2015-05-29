#ifndef ANOVAREPEATEDMEASURESFORM_H
#define ANOVAREPEATEDMEASURESFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"
#include "widgets/tablemodelanovadesign.h"
#include "widgets/tablemodelanovawithinsubjectcells.h"

namespace Ui {
class AnovaRepeatedMeasuresForm;
}

class AnovaRepeatedMeasuresForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaRepeatedMeasuresForm(QWidget *parent = 0);
	~AnovaRepeatedMeasuresForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	
private slots:
	void factorsChanging();
	void factorsChanged();
	void termsChanged();
	void withinSubjectsDesignChanged();

	void anovaDesignTableClicked(QModelIndex index);

private:
	Ui::AnovaRepeatedMeasuresForm *ui;

	TableModelAnovaDesign *_designTableModel;
	TableModelAnovaWithinSubjectCells *_withinSubjectCellsListModel;
	TableModelVariablesAssigned *_betweenSubjectsFactorsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_withinSubjectsTermsModel;
	TableModelAnovaModel *_betweenSubjectsTermsModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

	TableModelVariablesAvailable *_plotFactorsAvailableTableModel;
	TableModelVariablesAssigned *_horizontalAxisTableModel;
	TableModelVariablesAssigned *_seperateLinesTableModel;
	TableModelVariablesAssigned *_seperatePlotsTableModel;

};

#endif // ANOVAREPEATEDMEASURESFORM_H
