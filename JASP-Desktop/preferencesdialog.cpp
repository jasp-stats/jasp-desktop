#include "preferencesdialog.h"
#include "utils.h"
#include "ui_preferencesdialog.h"
#include <QMessageBox>
#include <QDebug>

using namespace std;

int PreferencesDialog::_currentTab = 0;

PreferencesDialog::PreferencesDialog(QWidget *parent) :
	QDialog(),
	ui(new Ui::PreferencesDialog)
{
	ui->setupUi(this);
	_tabBar = dynamic_cast<TabBar *>(parent);

	//Fix decimals
	int check_fix_decimals = _settings.value("fixDecimals", 0).toInt();
	ui->fixDecimals->setChecked(check_fix_decimals > 0);
	QString input_num_decimals = _settings.value("numDecimals").toString();
	if (input_num_decimals != "")
		ui->numDecimals->setValue(input_num_decimals.toInt());
	
	//Exact p-value
	int check_exact_pval = _settings.value("displayExactPVals", 0).toInt();
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

  //this->ui->numDecimals->setValidator(new QIntValidator(1,10, this));
  QSizePolicy retain = ui->numDecimals->sizePolicy();
  retain.setRetainSizeWhenHidden(true);
  ui->numDecimals->setSizePolicy(retain);
	if (check_fix_decimals == 0)
		this->ui->numDecimals->hide();

	connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(savePreferences()));
	connect(ui->useDefaultSpreadsheetEditor, SIGNAL(clicked(bool)), this, SLOT(setDefaultEditorCheck(bool)));
	connect(ui->openEditor, SIGNAL(pressed()),this, SLOT(getSpreadsheetEditor()));
	connect(ui->tabsPreferences, SIGNAL(currentChanged(int)), this, SLOT(currentTabChanged(int)));
	
	ui->tabsPreferences->setCurrentIndex(_currentTab);
}

PreferencesDialog::~PreferencesDialog()
{
	delete ui;
}

std::vector<std::string> PreferencesDialog::getStdVectorFromEmptyValueList()
{
	std::vector<std::string> result;
	QListWidgetItem *itm;
	unsigned int count = ui->missingValuesList->count();
	QString s;
	
	//Strip leading " and use template name for white spaces
	for (unsigned int i=0; i<count; ++i)
	{
		itm = ui->missingValuesList->item(i);
		s = itm->text();
		s = stripFirstAndLastChar(s, "\"");
		s = s.trimmed();
		if (!s.isEmpty())
			result.push_back(s.toStdString());
	}
	
	return result;
}

void PreferencesDialog::on_fixDecimals_clicked()
{

	if (ui->fixDecimals->checkState()==Qt::Checked)
	{
		this->ui->numDecimals->show();
	} 
	else
	{
		this->ui->numDecimals->hide();
	}
	
}

QString PreferencesDialog::getTokenStringFromEmptyValueList()
{
	QString result;
	QListWidgetItem *itm;
	unsigned int count = ui->missingValuesList->count();
	
	//No stripping is done
	for (unsigned int i=0; i<count; ++i)
	{
		itm = ui->missingValuesList->item(i);
		QString itemtoadd = itm->text();
		result += itemtoadd;
		if (i<count-1) result += "|";
	}
	
	return result;
	
}

bool PreferencesDialog::addStringToEmptyValueList(const QString &in)
{
	bool itemexist = false;
	
	QString prepare = in.trimmed();
	prepare = stripFirstAndLastChar(prepare, "\"");
	prepare = prepare.trimmed();

	if (!prepare.isEmpty())
	{
		for (unsigned int i=0; i<ui->missingValuesList->count(); ++i)
		{
			if (ui->missingValuesList->item(i)->text() == prepare)
			{
				itemexist = true;
				break;
			}
		}

		if (!itemexist)
		{
			ui->missingValuesList->addItem(prepare);
			ui->itemEdit->setText("");
		}
	}
	
	return itemexist;
}

void PreferencesDialog::on_addPushButton_clicked()
{

	addStringToEmptyValueList(ui->itemEdit->text());
	
}

void PreferencesDialog::on_deletePushButton_clicked()
{
	QListWidgetItem *itm = ui->missingValuesList->takeItem(ui->missingValuesList->currentRow());
	delete itm;
}

void PreferencesDialog::on_resetPushButton_clicked()
{
	fillMissingValueList(Utils::getDefaultEmptyValues());
}

void PreferencesDialog::currentTabChanged(int tabNr)
{
	_currentTab = tabNr;
}

void PreferencesDialog::checkEmptyValueList()
{
	// Forgot to save something?
	if (ui->itemEdit->text() != "")
	{
		QMessageBox::StandardButton reply;
		reply = QMessageBox::question(this, "", "Still one item to add to Missing Value List. Add ?",
									  QMessageBox::Yes|QMessageBox::No);
		if (reply == QMessageBox::Yes) 
			addStringToEmptyValueList(ui->itemEdit->text());
	}
		
	std::vector<std::string> missingvalues;
	QString settingmissingvalues;
	const std::vector<std::string> &org_missingvalues = Utils::getEmptyValues();

	settingmissingvalues = getTokenStringFromEmptyValueList();;
	missingvalues = getStdVectorFromEmptyValueList();
	
	if (settingmissingvalues != "" && missingvalues != org_missingvalues)
	{
		_settings.setValue("MissingValueList", settingmissingvalues);
		Utils::setEmptyValues(missingvalues);
		emit _tabBar->emptyValuesChanged();
	}
}

void PreferencesDialog::savePreferences()
{
	// Check empty value list
	checkEmptyValueList();

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
  int displayExactPVals = _settings.value("displayExactPVals", 1).toInt();
  _settings.setValue("displayExactPVals", checked);
  if (checked != displayExactPVals)
      _tabBar->setExactPValues(checked);

	//Fix decimals
  checked = (ui->fixDecimals->checkState()==Qt::Checked) ? 1 : 0;
	_settings.setValue("fixDecimals", checked);
	
	QString numDecimals = (checked == 0) ? "" : ui->numDecimals->cleanText();
	QString savedNumDecimals = _settings.value("numDecimals").toString();
	_settings.setValue("numDecimals", numDecimals);
	if (numDecimals != savedNumDecimals)
        _tabBar->setFixDecimals(numDecimals);
	
	//Done
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

void PreferencesDialog::showEvent(QShowEvent * event)
{
	fillMissingValueList(Utils::getEmptyValues());
	QDialog::showEvent(event);
}

void PreferencesDialog::fillMissingValueList(const vector<string> &emptyValues)
{
	ui->missingValuesList->clear();
	std::string s;

	for (unsigned int t=0; t<emptyValues.size() ; ++t)
	{
		s = emptyValues.at(t);
		ui->missingValuesList->addItem(s.c_str());
	}
}

