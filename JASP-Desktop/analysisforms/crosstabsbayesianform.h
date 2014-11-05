#ifndef CROSSTABSBAYESIANFORM_H
#define CROSSTABSBAYESIANFORM_H

#include <QWidget>
#include "analysisform.h"
#include "widgets/tablemodelvariableslevels.h"

namespace Ui {
class CrosstabsBayesianForm;
}

class CrosstabsBayesianForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit CrosstabsBayesianForm(QWidget *parent = 0);
	~CrosstabsBayesianForm();

private:
	Ui::CrosstabsBayesianForm *ui;

	TableModelVariablesAssigned *_rowsModel;
	TableModelVariablesAssigned *_columnsModel;
	TableModelVariablesAssigned *_countsModel;
	TableModelVariablesLevels *_layersModel;
};

#endif // CROSSTABSBAYESIANFORM_H
