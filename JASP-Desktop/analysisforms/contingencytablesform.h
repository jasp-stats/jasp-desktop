#ifndef CONTINGENCYTABLESFORM_H
#define CONTINGENCYTABLESFORM_H

#include <QWidget>
#include "analysisform.h"
#include "widgets/tablemodelvariableslevels.h"

namespace Ui {
class ContingencyTablesForm;
}

class ContingencyTablesForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit ContingencyTablesForm(QWidget *parent = 0);
	~ContingencyTablesForm();

private:
	Ui::ContingencyTablesForm *ui;

	TableModelVariablesAssigned *_rowsModel;
	TableModelVariablesAssigned *_columnsModel;
	TableModelVariablesAssigned *_countsModel;
	TableModelVariablesLevels *_layersModel;
};

#endif // CONTINGENCYTABLESFORM_H
