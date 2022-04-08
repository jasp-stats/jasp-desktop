#include "settings.h"
#include "enginedefinitions.h"

QSettings* Settings::_settings = nullptr;

const char *	Settings::defaultMissingValues = "NaN|nan|.|NA";

const Settings::Setting Settings::Values[] = {
	{"numDecimals",					3},
	{"exactPVals",					0},
	{"normalizedNotation",			0},
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
	{"resultFont",					"\"Lucida Grande\",Helvetica,Arial,sans-serif,\"Helvetica Neue\",freesans,Segoe UI"},
	{"win_LC_CTYPE_C",				"check" }, //"check" should be an actual value in the underlying enum that is defined in preferencesmodel.h
	{"maxEngineCount",				4		}, //In debug always 1
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
	{"dbImportInterval",			0		}
	
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

winLcCtypeSetting Settings::getWinLcCtypeSetting()
{
	QString lcCtypeSetting = Settings::value(Settings::LC_CTYPE_C_WIN).toString();
	
	winLcCtypeSetting val = winLcCtypeSetting::check;
	
	try
	{
		val = winLcCtypeSettingFromQString(lcCtypeSetting);	
	}
	catch(missingEnumVal & e) {} //Just keep it at check then
	
	return val;
}
