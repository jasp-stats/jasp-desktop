#ifndef LOCALESHELPERWINDOWS_H
#define LOCALESHELPERWINDOWS_H

#include <QObject>
#include <string>

class LocalesHelperWindows : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QStringList	locales			READ locales		WRITE setLocales		NOTIFY localesChanged		)
	Q_PROPERTY(QString		currentLocale	READ currentLocale	WRITE setCurrentLocale	NOTIFY currentLocaleChanged	)

public:
	explicit LocalesHelperWindows(QObject *parent = nullptr);

	static QString convertCodepagedStrToUtf8(const std::string & raw);

signals:

private:
	static LocalesHelperWindows * _singleton;

};

#endif // LOCALESHELPERWINDOWS_H
