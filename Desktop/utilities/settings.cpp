#include "settings.h"
#include "resultstesting/compareresults.h"
#include "gui/pdfdefinition.h"

static bool _thisIsATest = false;

void Settings::informSettingsThatThisIsATest()
{
	_thisIsATest = true;
}

QSettings* Settings::_settings = nullptr;

const char *	Settings::defaultEmptyValues = "NaN|nan|.|NA";

const Settings::Setting Settings::Values[] = {
	{"numDecimals",					3},
	{"exactPVals",					0},
	{"normalizedNotation",			true},
	{"useDefaultSpreadsheetEditor",	1},
	{"spreadsheetEditorName",		""},
	{"MissingValueList",			Settings::defaultEmptyValues},
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
	{"ThresholdScale",				10},
	{"logToFile",					false}, //By default do not log to file and when running debug-mode log to stdout and in release to nowhere.
	{"logFilesMax",					15},
	{"maxFlickVelocity",			800},
	{"modulesRemember",				true},
	{"modulesRemembered",			""},
	{"safeGraphicsMode",			false},
	{"cranRepositoryURL",			"https://cloud.r-project.org"},
	{"moduleLibraryURL",			"https://module-library.jasp-stats.org"},
	{"userHasGitHubAccount",		false},
	{"preferredLanguage",			"en"},
	{"preferredCountry",			QLocale::World},
	{"useAlternativeLocale",		true},
	{"alternativeLocLanguage",		QLocale(QLocale::English, QLocale::UnitedStates).nativeLanguageName() },
	{"alternativeLocRegion",		QLocale(QLocale::English, QLocale::UnitedStates).nativeTerritoryName() },
	{"useThousandSeparators",		false },
	{"themeName",					"lightTheme"},
	{"useNativeFileDialog",			true},
	{"disableAnimations",			false},
	{"generateMarkdownHelp",		false},
	{"interfaceFont",
#ifdef WIN32
									"Arial"	// https://github.com/jasp-stats/INTERNAL-jasp/issues/1146
#elif defined(__APPLE__)
									".AppleSystemUIFont"
#else
									"SansSerif"
#endif
	},
	{"codeFont",
#ifndef __APPLE__
									"Fira Code"
#else
									".AppleSystemUIFontMonospaced"
#endif
	},
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
	{"WindowsChosenCodepage",		-1		},// -1 is nothing chosen
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
	{"dataNALabel",					"."		},
	{"guiQtTextRender",				true	},
	{"showReports",					false	},
	{"showRSyntax",					false	},
	{"showAllROptions",				false	},
	{"showRSyntaxInResults",		false	},
	{"ALTNavModeActive",			true	},
	{"orderByValueByDefault",		true	},
	{"checkUpdatesAskUser",			true	},
	{"checkUpdates",				false	},
	{"checkUpdatesLastTime",		-1		},
	{"maxScaleLevels",				100		},
	{"pdfLandscape",				false	},
	{"pdfPageSize",					int(pdfPageSize::A4)			},
	{"directLibpathEnabled",		true	},
	{"directLibpathFolder",			""		},
	{"directDevModName",			""		},
	{"ribbonBarHeightScale",		1.0		},
#ifdef WIN32
    {"engineSandbox",				true	},
#else
    {"engineSandbox",				false	},
#endif
	{"remoteConfiguration",			false   },
	
	{"remoteConfigurationURL",		""		},
	{"localConfigurationPath",		""		},
	{"useConfigurationFile",		true	},
	{"startMaximized",				false	},
	{"storeStateEtc",				false	},
	{"showInteractiveDefault",		true	},
	{"autoSaveOn",					true	},
	{"autoSaveInterval",			5*60	},
};

QVariant Settings::value(Settings::Type key) {

	if(_thisIsATest && key == Settings::EMPTY_VALUES_LIST)
	{
		return QString(Settings::defaultEmptyValues) + "|Missing";
	}
	
	if(resultXmlCompare::compareResults::theOne()->testMode() || _thisIsATest)
  	switch(key)
		{
				default:                        return defaultValue(key);
				case Type::STORE_STATE_ETC:     return false; //Dont store state in the data library
		}
	
  QString settingStringName = Settings::Values[key].type;

#ifdef WIN32
    // 1. Enterprise Machine Policy (Strict GPO from IT Admins)
    QSettings gpoMachine("HKEY_LOCAL_MACHINE\\Software\\Policies\\JASP", QSettings::NativeFormat);
    if (gpoMachine.contains(settingStringName)) {
        return gpoMachine.value(settingStringName);
    }

    // 2. Enterprise User Policy (Strict GPO from IT Admins)
    QSettings gpoUser("HKEY_CURRENT_USER\\Software\\Policies\\JASP", QSettings::NativeFormat);
    if (gpoUser.contains(settingStringName)) {
        return gpoUser.value(settingStringName);
    }
#endif

    // 3. Current User Settings (Active INI)
    QSettings* settings = getSettings();
    if (settings->contains(settingStringName)) {
        return settings->value(settingStringName);
    }

#ifdef WIN32
    // 4. Legacy Migration (Old MSI User Preferences in HKCU)
    QSettings oldRegistry(QSettings::NativeFormat, QSettings::UserScope, "JASP", "JASP");
    if (oldRegistry.contains(settingStringName)) {
        QVariant oldVal = oldRegistry.value(settingStringName);
        
        // Migrate it to the new INI format
        settings->setValue(settingStringName, oldVal); 
        return oldVal;
    }
#endif

    // 5. Fallback to hardcoded application defaults
    return defaultValue(key);
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
