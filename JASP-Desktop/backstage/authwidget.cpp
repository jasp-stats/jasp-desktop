
#include "authwidget.h"
#include "ui_authwidget.h"

#include <QKeyEvent>

AuthWidget::AuthWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::AuthWidget)
{
	ui->setupUi(this);
	ui->RememberMeCheckBox->setChecked(_settings.value("OSFRememberMe", false).toBool());

	connect(ui->loginButton, SIGNAL(clicked(bool)), this, SLOT(loginSelected()));
	connect(ui->RememberMeCheckBox, SIGNAL(clicked(bool)), this, SLOT(on_RememberMeCheckBox_clicked(bool)));

	ui->email->installEventFilter(this);
	ui->password->installEventFilter(this);
}

AuthWidget::~AuthWidget()
{
	delete ui;
}

void AuthWidget::clearPassword()
{
	ui->password->setText("");
	ui->email->setSelection(0, ui->email->text().length());
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

void AuthWidget::on_RememberMeCheckBox_clicked(bool check)
{
	if (check)
		_settings.setValue("OSFRememberMe", check);
	else
		_settings.remove("OSFRememberMe");
}
