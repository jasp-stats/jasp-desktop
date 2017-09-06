#include "preferencesdialog.h"
#include "ui_preferencesdialog.h"
#include <QMessageBox>
#include <QDebug>

using namespace std;

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
	
	//Fill Empty Values List
	QString missingvaluestring;
	missingvaluestring = _settings.value("MissingValueList", "").toString();	
	ui->missingValuesList->clear();	
	if (missingvaluestring != "")
		setEmptValueListFromTokenString(missingvaluestring);		
	else
		setEmptValueListFromStdVector(Column::getEmptyValues());

	
	connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(savePreferences()));
	connect(ui->useDefaultSpreadsheetEditor, SIGNAL(clicked(bool)), this, SLOT(setDefaultEditorCheck(bool)));
	connect(ui->openEditor, SIGNAL(pressed()),this, SLOT(getSpreadsheetEditor()));
	
	ui->tabsPreferences->setCurrentIndex(0);
	

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
		if (s == SPACE_VALUE) s=" ";
		if (s == EMPTY_VALUE) s="";
		result.push_back(s.toStdString());
	}
	
	return result;
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

void PreferencesDialog::setEmptValueListFromStdVector(const std::vector<std::string> &inv)
{
	std::string s;
		
	for (unsigned int t=0; t<inv.size() ; ++t)
	{
		s = inv.at(t);
		if (s == " ") s = SPACE_VALUE;
		if (s == "") s = EMPTY_VALUE;
		ui->missingValuesList->addItem(s.c_str());
	}	
	
}

void PreferencesDialog::setEmptValueListFromTokenString(const QString &input)
{
	//Direct file from EmptyValuListString  into EmptyListControl	
	QStringList list;
	list = input.split("|");
	foreach (QString itm, list)
		ui->missingValuesList->addItem(itm);
	
}

bool PreferencesDialog::addStringToEmptyValueList(const QString &in)
{
	bool itemexist = false;
	
	QString prepare = in;
	prepare = stripFirstAndLastChar(prepare, "\"");
	if (prepare.length() == 0) prepare = EMPTY_VALUE;
	if (prepare.count(' ') == prepare.length()) prepare = SPACE_VALUE;
		
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
	ui->missingValuesList->clear();	
	setEmptValueListFromStdVector(Column::getDefaultEmptyValues());	
}

void PreferencesDialog::on_buttonBox_accepted()
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
	
	settingmissingvalues = getTokenStringFromEmptyValueList();;
	missingvalues = getStdVectorFromEmptyValueList();
	
	if (settingmissingvalues!="")
	{
		_settings.setValue("MissingValueList", settingmissingvalues);
		Column::setEmptyValues(missingvalues);
	}
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

