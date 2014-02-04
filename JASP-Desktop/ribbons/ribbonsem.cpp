#include "ribbonsem.h"
#include "ui_ribbonsem.h"

RibbonSEM::RibbonSEM(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonSEM)
{
	ui->setupUi(this);
}

RibbonSEM::~RibbonSEM()
{
	delete ui;
}
