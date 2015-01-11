
#include "appdirs.h"

#include <QApplication>
#include <QDir>
#include <QDebug>

const QString AppDirs::examples()
{
#ifdef __APPLE__
    static QString dir = QApplication::applicationDirPath() + "/../Resources/Data Sets";
#else
    static QString dir = QApplication::applicationDirPath() + QDir::separator() + "Data Sets";
#endif

	return dir;
}

const QString AppDirs::help()
{
#ifdef __APPLE__
	static QString dir = QApplication::applicationDirPath() + "/../Resources/Help";
#else
	static QString dir = QApplication::applicationDirPath() + QDir::separator() + "Help";
#endif

	return dir;
}

const QString AppDirs::tempDir()
{
	QString path = QDir::home().absoluteFilePath(".JASP");

	if (QDir(path).exists() == false)
	{
		if (QDir::home().mkdir(".JASP") == false)
			qDebug() << "temp dir could not be created :" << path;
	}

	return path;
}
