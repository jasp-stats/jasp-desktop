
#include "authwidget.h"
#include "ui_authwidget.h"

#include <QKeyEvent>

AuthWidget::AuthWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::AuthWidget)
{
	ui->setupUi(this);

	connect(ui->loginButton, SIGNAL(clicked(bool)), this, SLOT(loginSelected()));

	ui->email->installEventFilter(this);
	ui->password->installEventFilter(this);
}

AuthWidget::~AuthWidget()
{
	delete ui;
}

bool AuthWidget::eventFilter(QObject *object, QEvent *event)
{
	if (object == ui->password && event->type() == QEvent::KeyPress)
	{
		QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);

		switch (keyEvent->key())
		{
		case Qt::Key_Enter:
		case Qt::Key_Return:
			ui->loginButton->click();
		}
	}

	if (object == ui->email && event->type() == QEvent::Show)
	{
		ui->email->setFocus();
	}

	return QWidget::eventFilter(object, event);
}

void AuthWidget::loginSelected()
{
	emit loginRequested(ui->email->text(), ui->password->text());
}
