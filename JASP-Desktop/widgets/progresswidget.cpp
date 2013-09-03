#include "progresswidget.h"
#include "ui_progresswidget.h"

ProgressWidget::ProgressWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::ProgressWidget)
{
	ui->setupUi(this);
}

ProgressWidget::~ProgressWidget()
{
	delete ui;
}

void ProgressWidget::setStatus(const QString status, int progress)
{
	ui->labelStatus->setText(status);
	ui->progressBar->setValue(progress);
}
