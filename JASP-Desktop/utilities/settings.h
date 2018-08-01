#ifndef SETTINGS_H
#define SETTINGS_H

#include <QSettings>
#include <QString>

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
		TEST_ANALYSIS_QML,
		TEST_ANALYSIS_R
	};

	static QVariant value(Settings::Type key);
	static void setValue(Settings::Type key, const QVariant &value);
	static void sync();
	static void remove(Settings::Type key);
	static QSettings* getSettings();

private:
	struct Setting {
		QString type;
		QVariant defaultValue;
	};
	static QSettings* _settings;
	static const Setting Values[];

};
#endif // SETTINGS_H
