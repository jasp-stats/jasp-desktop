#include "settings.h"
#include "utilities/qutils.h"
#include "enginedefinitions.h"

QSettings* Settings::_settings = nullptr;

const char *	Settings::defaultMissingValues = "NaN|nan|.|NA";

const Settings::Setting Settings::Values[] = {
	{"numDecimals",					3},
	{"exactPVals",					0},
	{"normalizedNotation",			true},
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
	{"logToFile",					false}, //By default do not log to file and when running debug-mode log to stdout and in release to nowhere.
	{"logFilesMax",					15},
	{"maxFlickVelocity",			800},
	{"modulesRemember",				true},
	{"modulesRemembered",			""},
	{"safeGraphicsMode",			false},
	{"cranRepositoryURL",			"https://cloud.r-project.org"},
	{"userHasGitHubAccount",		false},
	{"preferredLanguage",			"en"},
	{"preferredCountry",			QLocale::World},
	{"themeName",					"lightTheme"},
	{"useNativeFileDialog",			true},
	{"disableAnimations",			false},
	{"generateMarkdownHelp",		false},
	{"interfaceFont",
#ifdef WIN32
									"Arial"},	// https://github.com/jasp-stats/INTERNAL-jasp/issues/1146
#elif defined(Q_OS_MACOS)
									".AppleSystemUIFont"},
#else
									"SansSerif"},
#endif
	{"codeFont",
#ifndef Q_OS_MACOS
									"Fira Code"},
#else
									".AppleSystemUIFontMonospaced"},
#endif
#ifdef WIN32
	{"resultFont",					"Arial,sans-serif,freesans,\"Segoe UI\""},
#elif __APPLE__
	{"resultFont",					"\"Lucida Grande\",Helvetica,Arial,sans-serif,\"Helvetica Neue\",freesans"},
#else // Linux and brave people compiling Jasp on other OSes
	{"resultFont",					"freesans,sans-serif"},
#endif
	{"maxEngineCount",				4		}, //In debug always 1
	{"maxEngineCountAdmin",			0		}, //If set to something >0 it will be the max allowed max engine count. This is here to allow admins to override the number of processes spawned as they might each consume quite some RAM.
	{"GITHUB_PAT_Custom",			""		},
	{"GITHUB_PAT_UseDefault",		true	},
	{"WindowsNoBomNative",			false	}, //false as default because then we keep the behaviour we had before.
	{"dbImportDbType",				0		},
	{"dbImportDbName",				""		},
	{"dbImportHostName",			""		},
	{"dbImportPort",				1433	},
	{"dbImportUserName",			""		},
	{"dbImportPassword",			""		},
	{"dbImportQuery",				""		},
	{"dbImportInterval",			0		},
	{"dbShowWarning",				true	},
	{"dbRememberMe",				false	},
	{"dataNALabel",					""		},
	{"guiQtTextRender",				true	},
	{"showReports",					false	}
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
