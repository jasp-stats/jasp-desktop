#include "optionsform.h"
#include "ui_optionsform.h"

OptionsForm::OptionsForm(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::OptionsForm)
{
	ui->setupUi(this);

	QVariant sem = _settings.value("plugins/sem", false);
	if (sem.canConvert(QVariant::Bool))
		ui->plugins_sem->setChecked(sem.toBool());

	connect(ui->plugins_sem, SIGNAL(clicked(bool)), this, SLOT(optionChangedHandler(bool)));
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
