#include "ribbonhome.h"
#include "ui_ribbonhome.h"

RibbonHome::RibbonHome(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonHome)
{
    ui->setupUi(this);


}

RibbonHome::~RibbonHome()
{
    delete ui;
}

void RibbonHome::frequenciesItemSelected()
{
    emit itemSelected(QString("base/frequencies"));
}

void RibbonHome::descriptivesItemSelected()
{
	emit itemSelected(QString("base/descriptives"));
}

void RibbonHome::anovaItemSelected()
{
	emit itemSelected(QString("anova"));
}
