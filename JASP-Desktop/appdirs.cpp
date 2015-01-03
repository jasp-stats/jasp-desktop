
#include "appdirs.h"

#include <QApplication>
#include <QDir>

const QString &AppDirs::examples()
{
#ifdef __APPLE__
    static QString dir = QApplication::applicationDirPath() + "/../Resources/Data Sets";
#else
    static QString dir = QApplication::applicationDirPath() + QDir::separator() + "Data Sets";
#endif

	return dir;
}

const QString &AppDirs::help()
{
#ifdef __APPLE__
	static QString dir = QApplication::applicationDirPath() + "/../Resources/Help";
#else
	static QString dir = QApplication::applicationDirPath() + QDir::separator() + "Help";
#endif

	return dir;
}
