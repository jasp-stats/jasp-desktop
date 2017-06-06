#include "preferencesdialog.h"
#include "ui_preferencesdialog.h"

PreferencesDialog::PreferencesDialog(QWidget *parent) :
	QDialog(),
	ui(new Ui::PreferencesDialog)
{
	ui->setupUi(this);
	_tabBar = dynamic_cast<TabBar *>(parent);


	//Exact p-value
	int check_exact_pval = _settings.value("exactPVals", 0).toInt();
	ui->displayExactPVals->setChecked(check_exact_pval > 0);

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

	// Remove Question mark Help sign (Only on windows )
	this->setWindowFlags(this->windowFlags() & ~Qt::WindowContextHelpButtonHint);

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
		
	//Auto Sync Switch
	int checked = (ui->syncAutoCheckBox->checkState()==Qt::Checked) ? 1 : 0;
	int dataAutoSynchronization = _settings.value("dataAutoSynchronization", 1).toInt();
	_settings.setValue("dataAutoSynchronization", checked);
	if (checked != dataAutoSynchronization)
		emit _tabBar->dataAutoSynchronizationChanged(checked);
	
	//Use Default Editor Switch
	checked = (ui->useDefaultSpreadsheetEditor->checkState()==Qt::Checked) ? 1 : 0;
	_settings.setValue("useDefaultSpreadsheetEditor", checked);

	_settings.setValue("spreadsheetEditorName", ui->spreadsheetEditorName->text());
			
	//Exact p values
	checked = (ui->displayExactPVals->checkState()==Qt::Checked) ? 1 : 0;
	_settings.setValue("exactPVals", checked);
	_tabBar->setExactPValues(checked);

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
	QString applicationfolder;

#ifdef __WIN32__
	applicationfolder = "c:\\Program Files";
#elif __APPLE__
	applicationfolder = "/Applications";
#else
	applicationfolder = "/usr/bin";
#endif

	QString filename = QFileDialog::getOpenFileName(this, "Select a file...", applicationfolder, filter);
	if (filename != "")
		ui->spreadsheetEditorName->setText(filename);
	
}

