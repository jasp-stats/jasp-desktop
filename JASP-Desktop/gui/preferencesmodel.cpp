#include "preferencesmodel.h"
#include "utils.h"
#include <QDebug>
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

QStringList PreferencesModel::missingValues()		const
{
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
		emit plotPPIChanged(defaultPPI());

}

void PreferencesModel::setWhiteBackground(bool newWhiteBackground)
{
	if (whiteBackground() == newWhiteBackground)
		return;

	Settings::setValue(Settings::IMAGE_BACKGROUND, newWhiteBackground ? "white" : "transparent");
	emit whiteBackgroundChanged(newWhiteBackground);
	emit plotBackgroundChanged(Settings::value(Settings::IMAGE_BACKGROUND).toString());
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

	if(!useDefaultPPI())
		emit plotPPIChanged(customPPI());
}

void PreferencesModel::setDefaultPPI(int defaultPPI)
{
	if (_defaultPPI == defaultPPI)
		return;

	_defaultPPI = defaultPPI;
	emit defaultPPIChanged(_defaultPPI);

	if(useDefaultPPI())
		emit plotPPIChanged(_defaultPPI);
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
