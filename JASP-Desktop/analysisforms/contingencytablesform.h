#ifndef CONTINGENCYTABLEFORM_H
#define CONTINGENCYTABLEFORM_H

#include "analysisform.h"

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

	ListModelVariablesAssigned *_rowsListModel;
	ListModelVariablesAssigned *_columnsListModel;
};

#endif // CONTINGENCYTABLEFORM_H
