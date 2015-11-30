#include "variableswidget.h"
#include "ui_variableswidget.h"

VariablesWidget::VariablesWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::VariablesWidget)
{
	ui->setupUi(this);
}

VariablesWidget::~VariablesWidget()
{
	delete ui;
}
