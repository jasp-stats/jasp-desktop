
#include "authwidget.h"
#include "ui_authwidget.h"

AuthWidget::AuthWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::AuthWidget)
{
	ui->setupUi(this);

	connect(ui->loginButton, SIGNAL(clicked(bool)), this, SLOT(loginSelected()));
}

AuthWidget::~AuthWidget()
{
	delete ui;
}

void AuthWidget::loginSelected()
{
	emit loginRequested(ui->email->text(), ui->password->text());
}
