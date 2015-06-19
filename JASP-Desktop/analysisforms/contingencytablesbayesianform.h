#ifndef CONTINGENCYTABLESBAYESIANFORM_H
#define CONTINGENCYTABLESBAYESIANFORM_H

#include <QWidget>
#include "analysisform.h"
#include "widgets/tablemodelvariableslevels.h"

namespace Ui {
class ContingencyTablesBayesianForm;
}

class ContingencyTablesBayesianForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit ContingencyTablesBayesianForm(QWidget *parent = 0);
	~ContingencyTablesBayesianForm();

private slots:
	void independentMultinomialSamplingToggled(bool on);
	void otherSamplingToggled(bool on);

private:
	Ui::ContingencyTablesBayesianForm *ui;

	TableModelVariablesAssigned *_rowsModel;
	TableModelVariablesAssigned *_columnsModel;
	TableModelVariablesAssigned *_countsModel;
	TableModelVariablesLevels *_layersModel;
};

#endif // CONTINGENCYTABLESBAYESIANFORM_H
