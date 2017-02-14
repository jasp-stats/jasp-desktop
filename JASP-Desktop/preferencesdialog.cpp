#include "preferencesdialog.h"
#include "ui_preferencesdialog.h"

PreferencesDialog::PreferencesDialog(QWidget *parent) :
	QDialog(parent),
	ui(new Ui::PreferencesDialog)
{
	
	ui->setupUi(this);
	
	//Auto Sync
	int check_data_sync = _settings.value("dataAutoSynchronization", 1).toInt();
	ui->syncAutoCheckBox->setChecked(check_data_sync > 0);
	
	//Default Editor
	int check_default_editor = _settings.value("useDefaultSpreadsheetEditor", 1).toInt();
	bool defaulteditor = check_default_editor > 0;
	ui->useDefaultSpreadsheetEditor->setChecked(defaulteditor);
	setDefaultEditorCheck(defaulteditor);
	
	//Selected Editor
	QString spreadsheetEditorName = _settings.value("spreadsheetEditorName","").toString();
	if (spreadsheetEditorName != "")
		ui->spreadsheetEditorName->setText(spreadsheetEditorName);

	connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(savePreferences()));
	connect(ui->useDefaultSpreadsheetEditor, SIGNAL(clicked(bool)), this, SLOT(setDefaultEditorCheck(bool)));
	connect(ui->openEditor, SIGNAL(pressed()),this, SLOT(getSpreadsheetEditor()));

}

PreferencesDialog::~PreferencesDialog()
{
	delete ui;
}

void PreferencesDialog::savePreferences()
{
	
	int checked;
	
	//Auto Sync Switch
	checked = (ui->syncAutoCheckBox->checkState()==Qt::Checked) ? 1 : 0;
	_settings.setValue("dataAutoSynchronization", checked);
	
	//Use Default Editor Switch
	checked = (ui->useDefaultSpreadsheetEditor->checkState()==Qt::Checked) ? 1 : 0;
	_settings.setValue("useDefaultSpreadsheetEditor", checked);
	
	_settings.setValue("spreadsheetEditorName", ui->spreadsheetEditorName->text());
			
	_settings.sync();
	this->close();
	
}

void PreferencesDialog::setDefaultEditorCheck(bool defaulteditor)
{
		
	if (defaulteditor)
	{
		ui->openEditor->setEnabled(false);
		ui->spreadsheetEditorName->setEnabled(false);
	}
	else
	{
		ui->openEditor->setEnabled(true);
		ui->openEditor->setDefault(true);
		ui->spreadsheetEditorName->setEnabled(true);
	}
	
}

void PreferencesDialog::getSpreadsheetEditor()
{
	
	QString filter = "File Description (*.*)";

	QString filename = QFileDialog::getOpenFileName(this, "Select a file...", "/Applications", filter);
	if (filename != "")
		ui->spreadsheetEditorName->setText(filename);
	
}

