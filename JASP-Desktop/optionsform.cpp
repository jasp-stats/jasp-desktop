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
