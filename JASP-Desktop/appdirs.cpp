
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

const QString &AppDirs::userGuide()
{
#ifdef __APPLE__
	static QString dir = QApplication::applicationDirPath() + "/../Resources/User Guide";
#else
	static QString dir = QApplication::applicationDirPath() + QDir::separator() + "User Guide";
#endif

	return dir;
}
