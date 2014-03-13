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

