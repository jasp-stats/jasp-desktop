//
// Copyright (C) 2013-2016 University of Amsterdam
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

#include "optionsform.h"
#include "ui_optionsform.h"

OptionsForm::OptionsForm(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::OptionsForm)
{
	ui->setupUi(this);

	QVariant v;

	v = _settings.value("plugins/sem", false);
	if (v.canConvert(QVariant::Bool))
		ui->plugins_sem->setChecked(v.toBool());

	v = _settings.value("toolboxes/r11tLearn", false);
	if (v.canConvert(QVariant::Bool))
		ui->toolboxes_r11tLearn->setChecked(v.toBool());

	connect(ui->plugins_sem, SIGNAL(clicked(bool)), this, SLOT(optionChangedHandler(bool)));
	connect(ui->toolboxes_r11tLearn, SIGNAL(clicked(bool)), this, SLOT(optionChangedHandler(bool)));

#ifdef QT_DEBUG
	ui->toolboxes_r11tLearn->setStyleSheet("QWidget { background-color: pink ; }");
#else
	ui->toolboxes_r11tLearn->hide();
#endif
}

OptionsForm::~OptionsForm()
{
	delete ui;
}

void OptionsForm::optionChangedHandler(bool newValue)
{
	QObject *source = this->sender();
	QString name = source->objectName().replace("_", "/");
	_settings.setValue(name, newValue);
	_settings.sync();

	emit optionsChanged();
}
