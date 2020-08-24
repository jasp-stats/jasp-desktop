#ifndef SETTINGS_H
#define SETTINGS_H

#include <QSettings>
#include <QString>
#include <QLocale>

class Settings {

public:
	enum Type {
		NUM_DECIMALS = 0,
		EXACT_PVALUES,
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
		DEVELOPER_MODE_REGENERATE_DESCRIPTION_ETC,
		LOG_TO_FILE,
		LOG_FILES_MAX,
		QML_MAX_FLICK_VELOCITY,
		MODULES_REMEMBER,
		MODULES_REMEMBERED,
		SAFE_GRAPHICS_MODE,
		CRAN_REPO_URL,
		USER_HAS_GITHUB_ACCOUNT,
		PREFERRED_LANGUAGE,
		THEME_NAME,
		USE_NATIVE_FILE_DIALOG,
		DISABLE_ANIMATIONS,
		GENERATE_MARKDOWN_HELP,
		INTERFACE_FONT,
		CONSOLE_FONT,
		RESULT_FONT,
		USE_DEFAULT_INTERFACE_FONT,
		USE_DEFAULT_CONSOLE_FONT,
		USE_DEFAULT_RESULT_FONT
	};

	static QVariant value(Settings::Type key);
	static QVariant defaultValue(Settings::Type key);
	static void setValue(Settings::Type key, const QVariant &value);
	static void sync();
	static void remove(Settings::Type key);
	static QSettings* getSettings();
	static const char *	defaultMissingValues;

private:
	struct Setting {
		QString type;
		QVariant defaultValue;
	};
	static QSettings* _settings;
	static const Setting Values[];

};
#endif // SETTINGS_H
