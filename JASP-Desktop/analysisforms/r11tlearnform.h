#ifndef R11TLEARNFORM_H
#define R11TLEARNFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"

namespace Ui {
class R11tLearnForm;
}

class R11tLearnForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit R11tLearnForm(QWidget *parent = 0);
	~R11tLearnForm();

private:
	Ui::R11tLearnForm *ui;

	TableModelVariablesAssigned *_subjectIdListModel;
	TableModelVariablesAssigned *_trialNumberListModel;
	TableModelVariablesAssigned *_rewardsListModel;
	TableModelVariablesAssigned *_lossesListModel;

};

#endif // R11TLEARNFORM_H
