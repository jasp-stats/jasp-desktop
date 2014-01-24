#include "crosstabsform.h"
#include "ui_crosstabsform.h"

CrosstabsForm::CrosstabsForm(QWidget *parent) :
	AnalysisForm("Crosstabs", parent),
	ui(new Ui::CrosstabsForm)
{
	ui->setupUi(this);
}

CrosstabsForm::~CrosstabsForm()
{
	delete ui;
}
