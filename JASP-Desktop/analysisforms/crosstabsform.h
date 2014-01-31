#ifndef CROSSTABSFORM_H
#define CROSSTABSFORM_H

#include <QWidget>
#include "analysisform.h"
#include "widgets/tablemodelvariableslevels.h"

namespace Ui {
class CrosstabsForm;
}

class CrosstabsForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit CrosstabsForm(QWidget *parent = 0);
	~CrosstabsForm();

private:
	Ui::CrosstabsForm *ui;

	ListModelVariablesAssigned *_rowsModel;
	ListModelVariablesAssigned *_columnsModel;
	TableModelVariablesLevels *_layersModel;
};

#endif // CROSSTABSFORM_H
