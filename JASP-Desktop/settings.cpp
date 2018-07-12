#include "settings.h"

QSettings* Settings::_settings = NULL;

const Settings::Setting Settings::Values[] = {
	{"numDecimals", ""},
	{"exactPVals", 0},
	{"dataAutoSynchronization", 1},
	{"useDefaultSpreadsheetEditor", 1},
	{"spreadsheetEditorName", ""},
	{"MissingValueList", ""},
	{"instructionsShown", false},
	{"recentFolders", ""},
	{"recentItems", ""},
	{"OSFUsername", ""},
	{"OSFPassword", ""},
	{"OSFEncryption", 0},
	{"OSFRememberMe", false},
	{"testAnalysisQML", ""},
	{"testAnalysisR", ""}
};

QVariant Settings::value(Settings::Type key)
{
	return getSettings()->value(Settings::Values[key].type, Settings::Values[key].defaultValue);
}

void Settings::setValue(Settings::Type key, const QVariant &value)
{
	getSettings()->setValue(Settings::Values[key].type, value);
}

void Settings::sync()
{
	getSettings()->sync();
}

void Settings::remove(Settings::Type key)
{
	getSettings()->remove(Settings::Values[key].type);
}

QSettings *Settings::getSettings()
{
	// if _settings is created immediately, it uses an invalid place to store its data.
	if (!_settings)
		_settings = new QSettings();
	return _settings;
}
