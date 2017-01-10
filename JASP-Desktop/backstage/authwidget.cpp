//
// Copyright (C) 2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "authwidget.h"
#include "ui_authwidget.h"

#include <QKeyEvent>
#include <QMessageBox>

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

void AuthWidget::setUsernameclearPassword()
{
	ui->password->setText("");
	QString username = _settings.value("OSFUsername", "").toString();
	ui->email->setText(username);
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
	if  ( ui->password->text()=="" || ui->email->text()=="" )
	{
		QMessageBox::warning(this, "Login", " User or password cannot be empty. ");
		return;
	}

	emit loginRequested(ui->email->text(), ui->password->text());
}

void AuthWidget::on_RememberMeCheckBox_clicked(bool check)
{
	if (check)
		_settings.setValue("OSFRememberMe", check);
	else
		_settings.remove("OSFRememberMe");
}
