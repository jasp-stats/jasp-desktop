#include "ribbonr11tlearn.h"
#include "ui_ribbonr11tlearn.h"

RibbonR11tLearn::RibbonR11tLearn(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonR11tLearn)
{
	ui->setupUi(this);
}

RibbonR11tLearn::~RibbonR11tLearn()
{
	delete ui;
}
