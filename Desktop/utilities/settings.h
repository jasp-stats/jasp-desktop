#ifndef SETTINGS_H
#define SETTINGS_H

#include <QSettings>
#include <QString>
#include <QLocale>

enum class winLcCtypeSetting;

///
/// Sometimes a class-name says more than a thousand comments
class Settings {

public:
	enum Type {
		NUM_DECIMALS = 0,
		EXACT_PVALUES,
		NORMALIZED_NOTATION,
		DATA_AUTO_SYNCHRONIZATION,
		USE_DEFAULT_SPREADSHEET_EDITOR,
		SPREADSHEET_EDITOR_NAME,
		MISSING_VALUES_LIST,
		INSTRUCTIONS_SHOWN,
		RECENT_FOLDERS,
		RECENT_ITEMS,
		OSF_USERNAME,
		OSF_PASSWORD,
		OSF_ENCRYPTION,
		OSF_REMEMBER_ME,
		OSF_SORTORDER,
		PPI_USE_DEFAULT,
		PPI_CUSTOM_VALUE,
		UI_SCALE,
		IMAGE_BACKGROUND,
		FIXED_DECIMALS,
		DEVELOPER_MODE,
		DEVELOPER_FOLDER,
		USE_CUSTOM_THRESHOLD_SCALE,
		THRESHOLD_SCALE,
		LOG_TO_FILE,
		LOG_FILES_MAX,
		QML_MAX_FLICK_VELOCITY,
		MODULES_REMEMBER,
		MODULES_REMEMBERED,
		SAFE_GRAPHICS_MODE,
		CRAN_REPO_URL,
		USER_HAS_GITHUB_ACCOUNT,
		PREFERRED_LANGUAGE,
		PREFERRED_COUNTRY,
		THEME_NAME,
		USE_NATIVE_FILE_DIALOG,
		DISABLE_ANIMATIONS,
		GENERATE_MARKDOWN_HELP,
		INTERFACE_FONT,
		CODE_FONT,
		RESULT_FONT,
		LC_CTYPE_C_WIN,
		MAX_ENGINE_COUNT,
		GITHUB_PAT_CUSTOM,
		GITHUB_PAT_USE_DEFAULT,
		WINDOWS_NO_BOM_NATIVE,
		DB_IMPORT_TYPE,
		DB_IMPORT_DBNAME,
		DB_IMPORT_HOSTNAME,
		DB_IMPORT_USERNAME,
		DB_IMPORT_PASSWORD
	};

	static QVariant value(Settings::Type key);
	static QVariant defaultValue(Settings::Type key);
	static void setValue(Settings::Type key, const QVariant &value);
	static void sync();
	static void remove(Settings::Type key);
	static QSettings* getSettings();
	static const char *	defaultMissingValues;
	
	static winLcCtypeSetting getWinLcCtypeSetting();

private:
	struct Setting {
		QString type;
		QVariant defaultValue;
	};
	static QSettings* _settings;
	static const Setting Values[];

};
#endif // SETTINGS_H
