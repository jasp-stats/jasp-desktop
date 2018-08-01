#include "preferencesdialog.h"
#include "utils.h"
#include "ui_preferencesdialog.h"
#include <QMessageBox>
#include <QDebug>
#include "utilities/settings.h"

using namespace std;

int PreferencesDialog::_currentTab = 0;

PreferencesDialog::PreferencesDialog(QWidget *parent) :
	QDialog(),
	ui(new Ui::PreferencesDialog)
{
	ui->setupUi(this);
	_tabBar = dynamic_cast<TabBar *>(parent);

	//Num decimals
	QString input_num_decimals = Settings::value(Settings::NUM_DECIMALS).toString();
	bool fix_decimals = !input_num_decimals.isEmpty();
	ui->fixDecimals->setChecked(fix_decimals);
	if (fix_decimals)
		ui->numDecimals->setValue(input_num_decimals.toInt());
	
	//Exact p-value
	bool check_exact_pval = Settings::value(Settings::EXACT_PVALUES).toBool();
	ui->displayExactPVals->setChecked(check_exact_pval);

	//Auto Sync
	bool check_data_sync = Settings::value(Settings::DATA_AUTO_SYNCHRONIZATION).toBool();
	ui->syncAutoCheckBox->setChecked(check_data_sync);

	//Default Editor
	bool defaultEditor = Settings::value(Settings::USE_DEFAULT_SPREADSHEET_EDITOR).toBool();
	ui->useDefaultSpreadsheetEditor->setChecked(defaultEditor);
	setDefaultEditorCheck(defaultEditor);
	
	//Selected Editor
	QString spreadsheetEditorName = Settings::value(Settings::SPREADSHEET_EDITOR_NAME).toString();
	if (spreadsheetEditorName != "")
		ui->spreadsheetEditorName->setText(spreadsheetEditorName);

	QString testAnalyseQMLName = Settings::value(Settings::TEST_ANALYSIS_QML).toString();
	if (testAnalyseQMLName != "")
		ui->testAnalyseQMLName->setText(testAnalyseQMLName);
	
	QString testAnalyseRName = Settings::value(Settings::TEST_ANALYSIS_R).toString();
	if (testAnalyseRName != "")
		ui->testAnalyseRName->setText(testAnalyseRName);
	
	// Remove Question mark Help sign (Only on windows )
	this->setWindowFlags(this->windowFlags() & ~Qt::WindowContextHelpButtonHint);

  QSizePolicy retain = ui->numDecimals->sizePolicy();
  retain.setRetainSizeWhenHidden(true);
  ui->numDecimals->setSizePolicy(retain);
	if (!fix_decimals)
		this->ui->numDecimals->hide();

	connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(savePreferences()));
	connect(ui->useDefaultSpreadsheetEditor, SIGNAL(clicked(bool)), this, SLOT(setDefaultEditorCheck(bool)));
	connect(ui->openEditor, SIGNAL(pressed()),this, SLOT(getSpreadsheetEditor()));
	connect(ui->openQMLFile, SIGNAL(pressed()),this, SLOT(getQMLFile()));
	connect(ui->openRFile, SIGNAL(pressed()),this, SLOT(getRFile()));
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
		for (int i=0; i<ui->missingValuesList->count(); ++i)
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
		Settings::setValue(Settings::MISSING_VALUES_LIST, settingmissingvalues);
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
	int dataAutoSynchronization = Settings::value(Settings::DATA_AUTO_SYNCHRONIZATION).toInt();
	Settings::setValue(Settings::DATA_AUTO_SYNCHRONIZATION, checked);
	if (checked != dataAutoSynchronization)
		emit _tabBar->dataAutoSynchronizationChanged(checked);
	
	//Use Default Editor Switch
	checked = (ui->useDefaultSpreadsheetEditor->checkState()==Qt::Checked) ? 1 : 0;
	Settings::setValue(Settings::USE_DEFAULT_SPREADSHEET_EDITOR, checked);

	Settings::setValue(Settings::SPREADSHEET_EDITOR_NAME, ui->spreadsheetEditorName->text());
			
	//Exact p values
	checked = (ui->displayExactPVals->checkState()==Qt::Checked) ? 1 : 0;
	int displayExactPVals = Settings::value(Settings::EXACT_PVALUES).toInt();
	Settings::setValue(Settings::EXACT_PVALUES, checked);
	if (checked != displayExactPVals)
		_tabBar->setExactPValues(checked);

	//Fix decimals
	checked = (ui->fixDecimals->checkState()==Qt::Checked) ? 1 : 0;	
	QString numDecimals = (checked == 0) ? "" : ui->numDecimals->cleanText();
	QString savedNumDecimals = Settings::value(Settings::NUM_DECIMALS).toString();
	Settings::setValue(Settings::NUM_DECIMALS, numDecimals);
	if (numDecimals != savedNumDecimals)
        _tabBar->setFixDecimals(numDecimals);
	
	Settings::setValue(Settings::TEST_ANALYSIS_QML, ui->testAnalyseQMLName->text());
	Settings::setValue(Settings::TEST_ANALYSIS_R, ui->testAnalyseRName->text());
	//Done
	Settings::sync();

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

void PreferencesDialog::getQMLFile()
{
	QString filter = "File Description (*.qml)";
	QString folder;

#ifdef __WIN32__
	folder = "c:\\";
#elif __APPLE__
	folder = "~/Documenents";
#else
	folder = "/";
#endif

	QString filename = QFileDialog::getOpenFileName(this, "Select a file...", folder, filter);
	if (filename != "")
		ui->testAnalyseQMLName->setText(filename);	
}

void PreferencesDialog::getRFile()
{
	QString filter = "File Description (*.R)";
	QString folder;

#ifdef __WIN32__
	folder = "c:\\";
#elif __APPLE__
	folder = "~/Documenents";
#else
	folder = "/";
#endif

	QString filename = QFileDialog::getOpenFileName(this, "Select a file...", folder, filter);
	if (filename != "")
		ui->testAnalyseRName->setText(filename);	
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

