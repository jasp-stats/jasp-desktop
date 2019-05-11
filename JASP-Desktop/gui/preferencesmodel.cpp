#include "preferencesmodel.h"
#include "utils.h"

#include "utilities/settings.h"
#include "gui/messageforwarder.h"

using namespace std;

PreferencesModel::PreferencesModel(QObject *parent) :
	QObject(parent)
{
	connect(this, &PreferencesModel::missingValuesChanged, this, &PreferencesModel::updateUtilsMissingValues);
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

void PreferencesModel::browseSpreadsheetEditor()
{
	
	QString filter = "File Description (*.*)";
	QString applicationfolder;

#ifdef _WIN32
	applicationfolder = "c:\\Program Files";
#elif __APPLE__
	applicationfolder = "/Applications";
#else
	applicationfolder = "/usr/bin";
#endif

	QString filename = MessageForwarder::browseOpenFile("Select a file...", applicationfolder, filter);

	if (filename != "")
		setCustomEditor(filename);
	
}

void PreferencesModel::browseDeveloperFolder()
{
	QString defaultfolder = developerFolder();
	if (defaultfolder.isEmpty())
	{
#ifdef _WIN32
		defaultfolder = "c:\\";
#else
		defaultfolder = "~";
#endif
	}

	QString folder = MessageForwarder::browseOpenFolder("Select a folder...", defaultfolder);

	if (!folder.isEmpty())
		setDeveloperFolder(folder);
		
}

bool	PreferencesModel::fixedDecimals()			const { return Settings::value(Settings::FIXED_DECIMALS								).toBool();					}
int		PreferencesModel::numDecimals()				const { return Settings::value(Settings::NUM_DECIMALS								).toInt();					}
bool	PreferencesModel::exactPValues()			const { return Settings::value(Settings::EXACT_PVALUES								).toBool();					}
bool	PreferencesModel::dataAutoSynchronization()	const { return Settings::value(Settings::DATA_AUTO_SYNCHRONIZATION					).toBool();					}
bool	PreferencesModel::useDefaultEditor()		const { return Settings::value(Settings::USE_DEFAULT_SPREADSHEET_EDITOR				).toBool();					}
QString	PreferencesModel::customEditor()			const { return Settings::value(Settings::SPREADSHEET_EDITOR_NAME					).toString();				}
QString PreferencesModel::developerFolder()			const {	return Settings::value(Settings::DEVELOPER_FOLDER							).toString();				}
bool	PreferencesModel::useDefaultPPI()			const { return Settings::value(Settings::PPI_USE_DEFAULT							).toBool();					}
int		PreferencesModel::customPPI()				const { return Settings::value(Settings::PPI_CUSTOM_VALUE							).toInt();					}
bool	PreferencesModel::whiteBackground()			const { return Settings::value(Settings::IMAGE_BACKGROUND							).toString() == "white";	}
bool	PreferencesModel::developerMode()			const { return Settings::value(Settings::DEVELOPER_MODE								).toBool();					}
double	PreferencesModel::uiScale()					const { return Settings::value(Settings::UI_SCALE									).toDouble();				}
bool	PreferencesModel::customThresholdScale()	const { return Settings::value(Settings::USE_CUSTOM_THRESHOLD_SCALE					).toBool();					}
int		PreferencesModel::thresholdScale()			const { return Settings::value(Settings::THRESHOLD_SCALE							).toInt();					}
bool	PreferencesModel::devModRegenDESC()			const { return Settings::value(Settings::DEVELOPER_MODE_REGENERATE_DESCRIPTION_ETC	).toBool();					}
bool	PreferencesModel::logToFile()				const {	return Settings::value(Settings::LOG_TO_FILE								).toBool();					}
int		PreferencesModel::logFilesMax()				const {	return Settings::value(Settings::LOG_FILES_MAX								).toInt();					}

QStringList PreferencesModel::missingValues()		const
{;
	QStringList items = Settings::value(Settings::MISSING_VALUES_LIST).toString().split("|");

	return items;
}

QString PreferencesModel::fixedDecimalsForJS() const
{
	if(!fixedDecimals())
		return "\"\"";

	return QString::fromStdString(std::to_string(numDecimals()));
}

void PreferencesModel::setFixedDecimals(bool newFixedDecimals)
{
	if (fixedDecimals() == newFixedDecimals)
		return;

	Settings::setValue(Settings::FIXED_DECIMALS, newFixedDecimals);

	emit fixedDecimalsChanged(newFixedDecimals);
	emit fixedDecimalsChangedString(fixedDecimalsForJS());
}

void PreferencesModel::setNumDecimals(int newNumDecimals)
{
	if (numDecimals() == newNumDecimals)
		return;

	Settings::setValue(Settings::NUM_DECIMALS, newNumDecimals);

	emit numDecimalsChanged(newNumDecimals);

	if(fixedDecimals())
		emit fixedDecimalsChangedString(fixedDecimalsForJS());
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

	if(customPPI() != defaultPPI())
		emit plotPPIChanged(plotPPI());
	
}

void PreferencesModel::setDeveloperMode(bool newDeveloperMode)
{
	if (developerMode() == newDeveloperMode)
		return;

	Settings::setValue(Settings::DEVELOPER_MODE, newDeveloperMode);
	emit developerModeChanged(newDeveloperMode);
	
}

void PreferencesModel::setWhiteBackground(bool newWhiteBackground)
{
	if (whiteBackground() == newWhiteBackground)
		return;

	Settings::setValue(Settings::IMAGE_BACKGROUND, newWhiteBackground ? "white" : "transparent");
	emit whiteBackgroundChanged(newWhiteBackground);
	emit plotBackgroundChanged(Settings::value(Settings::IMAGE_BACKGROUND).toString());
}

void PreferencesModel::setDeveloperFolder(QString newDeveloperFolder)
{
	if (developerFolder() == newDeveloperFolder)
		return;

	Settings::setValue(Settings::DEVELOPER_FOLDER, newDeveloperFolder);
	emit developerFolderChanged(newDeveloperFolder);
}

void PreferencesModel::setUiScale(double newUiScale)
{
	newUiScale = std::max(0.2, newUiScale);

	if (std::abs(uiScale() - newUiScale) < 0.001)
		return;

	Settings::setValue(Settings::UI_SCALE, newUiScale);
	emit uiScaleChanged(newUiScale);
}

void PreferencesModel::zoomIn()
{
	setUiScale(uiScale() + 0.1);
}

void PreferencesModel::zoomOut()
{
	if (uiScale() >= 0.2)
		setUiScale(uiScale() - 0.1);
}

void PreferencesModel::zoomReset()
{
	setUiScale(1.0);
}

void PreferencesModel::setCustomPPI(int newCustomPPI)
{
	if (customPPI() == newCustomPPI)
		return;

	Settings::setValue(Settings::PPI_CUSTOM_VALUE, newCustomPPI);
	emit customPPIChanged(newCustomPPI);

	if(!useDefaultPPI())
		emit plotPPIChanged(plotPPI());
}

void PreferencesModel::setDefaultPPI(int defaultPPI)
{
	if (_defaultPPI == defaultPPI)
		return;

	_defaultPPI = defaultPPI;
	emit defaultPPIChanged(_defaultPPI);

	if(useDefaultPPI())
		emit plotPPIChanged(plotPPI());
}

void PreferencesModel::removeMissingValue(QString value)
{
	QStringList currentValues = missingValues();
	if(currentValues.contains(value))
	{
		currentValues.removeAll(value);
		Settings::setValue(Settings::MISSING_VALUES_LIST, currentValues.join("|"));
		emit missingValuesChanged();
	}
}

void PreferencesModel::addMissingValue(QString value)
{
	{
		QStringList currentValues = missingValues();
		if(!currentValues.contains(value))
		{
			currentValues.append(value);
			Settings::setValue(Settings::MISSING_VALUES_LIST, currentValues.join("|"));
			emit missingValuesChanged();
		}
	}
}

void PreferencesModel::resetMissingValues()
{
	QStringList currentValues = missingValues();
	Settings::setValue(Settings::MISSING_VALUES_LIST, Settings::defaultMissingValues);

	if(missingValues() != currentValues)
		emit missingValuesChanged();
}

void PreferencesModel::setCustomThresholdScale(bool newCustomThresholdScale)
{
	if (customThresholdScale() == newCustomThresholdScale)
		return;

	Settings::setValue(Settings::USE_CUSTOM_THRESHOLD_SCALE, newCustomThresholdScale);
	emit customThresholdScaleChanged (newCustomThresholdScale);
}

void PreferencesModel::setThresholdScale(int newThresholdScale)
{
	if (thresholdScale() == newThresholdScale)
		return;

	Settings::setValue(Settings::THRESHOLD_SCALE, newThresholdScale);
	emit thresholdScaleChanged(newThresholdScale);

}

void PreferencesModel::updateUtilsMissingValues()
{
	missingValuesToStdVector(Utils::_currentEmptyValues);
	Utils::processEmptyValues();
}

void PreferencesModel::missingValuesToStdVector(std::vector<std::string> & out)	const
{
	QStringList currentValues = missingValues();

	out.resize(size_t(currentValues.size()));

	for(size_t i=0; i<out.size(); i++)
		out[i] = currentValues[int(i)].toStdString();
}

void PreferencesModel::setDevModRegenDESC(bool newDevModRegenDESC)
{
	if (devModRegenDESC() == newDevModRegenDESC)
		return;

	Settings::setValue(Settings::DEVELOPER_MODE_REGENERATE_DESCRIPTION_ETC, newDevModRegenDESC);
	emit devModRegenDESCChanged(newDevModRegenDESC);
}

void PreferencesModel::setLogToFile(bool newLogToFile)
{
	if (logToFile() == newLogToFile)
		return;

	Settings::setValue(Settings::LOG_TO_FILE, newLogToFile);
	emit logToFileChanged(newLogToFile);
}

void PreferencesModel::setLogFilesMax(int newLogFilesMax)
{
	if (logFilesMax() == newLogFilesMax)
		return;

	Settings::setValue(Settings::LOG_FILES_MAX, newLogFilesMax);
	emit logFilesMaxChanged(newLogFilesMax);
}
