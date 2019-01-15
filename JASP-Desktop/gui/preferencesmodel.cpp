#include "preferencesmodel.h"
#include "utils.h"
#include <QDebug>
#include "utilities/settings.h"
#include "gui/messageforwarder.h"

using namespace std;

PreferencesModel::PreferencesModel(QObject *parent) :
	QObject(parent)
{
	fillMissingValueList(Utils::getEmptyValues());
}

PreferencesModel::~PreferencesModel()
{

}

/*
const float sliderMult = 0.33333f;


void PreferencesModel::setSliderUIScale(float scale)
{
	float	scaleX		= std::log((scale) / sliderMult) * 32.0f;
	int		minSlider	= ui->sliderUIScale->minimum(),
			maxSlider	= ui->sliderUIScale->maximum(),
			newPos		= std::max(minSlider, std::min(maxSlider, int(scaleX)));

	ui->sliderUIScale->setValue(newPos);
}

float PreferencesModel::sliderUIScale()
{
	//float curPos = ui->sliderUIScale->value();

	//return std::exp(curPos / 32.0f) * sliderMult;
	return -1;
}

void PreferencesModel::sliderUIScaleChanged(int)
{
	float scale = sliderUIScale();
	Settings::setValue(Settings::UI_SCALE, scale);
	emit _tabBar->UIScaleChanged(scale);
}
*/

std::vector<std::string> PreferencesModel::getStdVectorFromEmptyValueList()
{
	std::vector<std::string> result;
/*	QListWidgetItem *itm;
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
	}*/
	
	return result;
}

QString PreferencesModel::getTokenStringFromEmptyValueList()
{
	QString result;
	/*QListWidgetItem *itm;
	unsigned int count = ui->missingValuesList->count();
	
	//No stripping is done
	for (unsigned int i=0; i<count; ++i)
	{
		itm = ui->missingValuesList->item(i);
		QString itemtoadd = itm->text();
		result += itemtoadd;
		if (i<count-1) result += "|";
	}
	*/
	return result;
	
}

bool PreferencesModel::addStringToEmptyValueList(const QString &in)
{
	bool itemexist = false;
/*
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
	*/
	return itemexist;
}

void PreferencesModel::checkEmptyValueList()
{
/*	// Forgot to save something?
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
	}*/
}

void PreferencesModel::savePreferences()
{
/*	// Check empty value list
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

	//PPI
	checked = (ui->useDefaultPPI->checkState()==Qt::Checked) ? 1 : 0;
	int previousChecked = Settings::value(Settings::PPI_USE_DEFAULT).toInt();
	Settings::setValue(Settings::PPI_USE_DEFAULT, checked);
	if (checked)
	{
		if (checked != previousChecked)
			_tabBar->useDefaultPPI();
	}
	else
	{
		int customPPI = ui->customPPI->text().toInt();
		int previousCustomPPI = Settings::value(Settings::PPI_CUSTOM_VALUE).toInt();
		Settings::setValue(Settings::PPI_CUSTOM_VALUE, customPPI);
		if (checked != previousChecked || customPPI != previousCustomPPI)
			_tabBar->setPPI(customPPI);
	}

	//ImageBackground
	QAbstractButton* imageBackgroundButton = _imageBackgroundGroup->checkedButton();
	QString imageBackgroundValue = imageBackgroundButton->objectName().remove("Background");
	QString currentImageBackgroundValue = Settings::value(Settings::IMAGE_BACKGROUND).toString();
	if (imageBackgroundValue != currentImageBackgroundValue)
	{
		Settings::setValue(Settings::IMAGE_BACKGROUND, imageBackgroundValue);
		_tabBar->setImageBackground(imageBackgroundValue);
	}


	Settings::setValue(Settings::TEST_ANALYSIS_QML, ui->testAnalyseQMLName->text());
	Settings::setValue(Settings::TEST_ANALYSIS_R, ui->testAnalyseRName->text());

	//Done
	Settings::sync();

	this->close();*/
	
}

void PreferencesModel::browseSpreadsheetEditor()
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

	QString filename = MessageForwarder::openFileBrowse("Select a file...", applicationfolder, filter);

	if (filename != "")
		setCustomEditor(filename);
	
}


void PreferencesModel::fillMissingValueList(const vector<string> &emptyValues)
{
/*	ui->missingValuesList->clear();
	std::string s;

	for (unsigned int t=0; t<emptyValues.size() ; ++t)
	{
		s = emptyValues.at(t);
		ui->missingValuesList->addItem(s.c_str());
	}*/
}

bool	PreferencesModel::fixedDecimals()			const { return Settings::value(Settings::FIXED_DECIMALS					).toBool();					}
int		PreferencesModel::numDecimals()				const { return Settings::value(Settings::NUM_DECIMALS					).toInt();					}
bool	PreferencesModel::exactPValues()			const { return Settings::value(Settings::EXACT_PVALUES					).toBool();					}
bool	PreferencesModel::dataAutoSynchronization()	const { return Settings::value(Settings::DATA_AUTO_SYNCHRONIZATION		).toBool();					}
bool	PreferencesModel::useDefaultEditor()		const { return Settings::value(Settings::USE_DEFAULT_SPREADSHEET_EDITOR	).toBool();					}
QString	PreferencesModel::customEditor()			const { return Settings::value(Settings::SPREADSHEET_EDITOR_NAME		).toString();				}
bool	PreferencesModel::useDefaultPPI()			const { return Settings::value(Settings::PPI_USE_DEFAULT				).toBool();					}
int		PreferencesModel::customPPI()				const { return Settings::value(Settings::PPI_CUSTOM_VALUE				).toInt();					}
bool	PreferencesModel::whiteBackground()			const { return Settings::value(Settings::IMAGE_BACKGROUND				).toString() == "white";	}
double	PreferencesModel::uiScale()					const { return Settings::value(Settings::UI_SCALE						).toDouble();				}

void PreferencesModel::setFixedDecimals(bool newFixedDecimals)
{
	if (fixedDecimals() == newFixedDecimals)
		return;

	Settings::setValue(Settings::FIXED_DECIMALS, newFixedDecimals);

	emit fixedDecimalsChanged(newFixedDecimals);
}

void PreferencesModel::setNumDecimals(int newNumDecimals)
{
	if (numDecimals() == newNumDecimals)
		return;

	Settings::setValue(Settings::NUM_DECIMALS, newNumDecimals);

	emit numDecimalsChanged(newNumDecimals);
}


void PreferencesModel::setExactPValues(bool newExactPValues)
{
	if (exactPValues() == newExactPValues)
		return;

	Settings::setValue(Settings::EXACT_PVALUES, newExactPValues);

	emit exactPValuesChanged(newExactPValues);
}



void PreferencesModel::setDataAutoSynchronization(bool newDataAutoSynchronization)
{
	if (dataAutoSynchronization() == newDataAutoSynchronization)
		return;

	Settings::setValue(Settings::DATA_AUTO_SYNCHRONIZATION, newDataAutoSynchronization);
	emit dataAutoSynchronizationChanged(newDataAutoSynchronization);
}

void PreferencesModel::setUseDefaultEditor(bool newUseDefaultEditor)
{
	if (useDefaultEditor() == newUseDefaultEditor)
		return;

	Settings::setValue(Settings::USE_DEFAULT_SPREADSHEET_EDITOR, newUseDefaultEditor);
	emit useDefaultEditorChanged(newUseDefaultEditor);
}

void PreferencesModel::setCustomEditor(QString newCustomEditor)
{
	if (customEditor() == newCustomEditor)
		return;

	Settings::setValue(Settings::SPREADSHEET_EDITOR_NAME, newCustomEditor);
	emit customEditorChanged(newCustomEditor);
}

void PreferencesModel::setUseDefaultPPI(bool newUseDefaultPPI)
{
	if (useDefaultPPI() == newUseDefaultPPI)
		return;

	Settings::setValue(Settings::PPI_USE_DEFAULT, newUseDefaultPPI);
	emit useDefaultPPIChanged(newUseDefaultPPI);
}

void PreferencesModel::setWhiteBackground(bool newWhiteBackground)
{
	if (whiteBackground() == newWhiteBackground)
		return;

	Settings::setValue(Settings::IMAGE_BACKGROUND, newWhiteBackground ? "white" : "transparent");
	emit whiteBackgroundChanged(newWhiteBackground);
}

void PreferencesModel::setUiScale(double newUiScale)
{
	if (std::abs(uiScale() - newUiScale) < 0.001)
		return;

	Settings::setValue(Settings::UI_SCALE, newUiScale);
	emit uiScaleChanged(newUiScale);
}

void PreferencesModel::setCustomPPI(int newCustomPPI)
{
	if (customPPI() == newCustomPPI)
		return;

	Settings::setValue(Settings::PPI_CUSTOM_VALUE, newCustomPPI);
	emit customPPIChanged(newCustomPPI);
}

