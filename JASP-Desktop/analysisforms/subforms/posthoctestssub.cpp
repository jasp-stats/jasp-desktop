#include "posthoctestssub.h"
#include "ui_posthoctestssub.h"

PostHocTestsSub::PostHocTestsSub(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::PostHocTestsSub)
{
	ui->setupUi(this);
}

PostHocTestsSub::~PostHocTestsSub()
{
	delete ui;
}
