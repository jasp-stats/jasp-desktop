#include "semsimpleform.h"
#include "ui_semsimpleform.h"

#include "widgets/itemmodelselectvariable.h"

SEMSimpleForm::SEMSimpleForm(QWidget *parent) :
	AnalysisForm("SEMSimpleForm", parent),
	ui(new Ui::SEMSimpleForm)
{
	ui->setupUi(this);

	ItemModelSelectVariable *model = new ItemModelSelectVariable(this);
	model->setSource(&_availableFields);

	ui->groupingVariable->setModel(model);

	ui->containerStatistics->hide();
	ui->containerOptions->hide();
	ui->containerAdvanced->hide();
}

SEMSimpleForm::~SEMSimpleForm()
{
	delete ui;
}
