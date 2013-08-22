#include "ttestpairedsamplesform.h"
#include "ui_ttestpairedsamplesform.h"

TTestPairedSamplesForm::TTestPairedSamplesForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestPairedSamplesForm)
{
	ui->setupUi(this);

	ui->availableFields->setModel(&_availableFields);
	ui->availableFields->addAssignButton(ui->assignButton);

	ui->pairs->setAssignButton(ui->assignButton);
	ui->pairs->setAvailableFieldsListView(ui->availableFields);


}

TTestPairedSamplesForm::~TTestPairedSamplesForm()
{
	delete ui;
}
