#include "settings.h"

QSettings* Settings::_settings = nullptr;

const char *	Settings::defaultMissingValues = "NaN|nan|.|NA";

const Settings::Setting Settings::Values[] = {
	{"numDecimals",					3},
	{"exactPVals",					0},
	{"dataAutoSynchronization",		1},
	{"useDefaultSpreadsheetEditor",	1},
	{"spreadsheetEditorName",		""},
	{"MissingValueList",			Settings::defaultMissingValues},
	{"instructionsShown",			false},
	{"recentFolders",				""},
	{"recentItems",					""},
	{"OSFUsername",					""},
	{"OSFPassword",					""},
	{"OSFEncryption",				0},
	{"OSFRememberMe",				false},
	{"OSFSortOrder",				0},
	{"PPIUseDefault",				false},
	{"PPICustomValue",				300},
	{"UIScaleAll",					1.0f},
	{"ImageBackground",				"white"},
	{"fixedDecimals",				false},
	{"developerMode",				false},
	{"developerFolder",				""},
	{"CustomThresholdScale",		false},
	{"ThresholdScale",				10},
	{"devModeRegenDescEtc",			true},
	{"logToFile",					false}, //By default do not log to file and when running debug-mode log to stdout and in release to nowhere.
	{"logFilesMax",					15},
	{"maxFlickVelocity",			800},
	{"modulesRemember",				true},
	{"modulesRemembered",			""},
#ifdef __APPLE__
	{"safeGraphicsModeMac",			true}, //Renamed and turned on to make mac always start in safe graphics mode by default to avoid (as much as possible) the crashes on mac reported in https://github.com/jasp-stats/INTERNAL-jasp/issues/720
#else
	{"safeGraphicsMode",			false},
#endif
	{"cranRepositoryURL",			"https://cloud.r-project.org"},
	{"userHasGitHubAccount",		false},
	{"preferredLanguage",			QLocale::English},
	{"themeName",					"lightTheme"},
	{"useNativeFileDialog",			true},
	{"disableAnimations",			false}
};

QVariant Settings::value(Settings::Type key)
{
	return getSettings()->value(Settings::Values[key].type, Settings::Values[key].defaultValue);
}

QVariant Settings::defaultValue(Settings::Type key)
{
	return Settings::Values[key].defaultValue;
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
	// if _settings is created immediately, it does not use the right place to store its data.
	if (!_settings)
		_settings = new QSettings();
	return _settings;
}
