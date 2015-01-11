
#include "appdirs.h"

#include <QApplication>
#include <QDir>
#include <QDebug>

#ifdef __WIN32__
#include <windows.h>
#include "qutils.h"
#include "utils.h"
#endif

using namespace std;

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
		if (QDir::home().mkdir(".JASP"))
		{
#ifdef __WIN32__

			wstring wpath = Utils::s2ws(fq(path));

			DWORD attributes;

			attributes = GetFileAttributes(wpath.c_str());
			attributes |= FILE_ATTRIBUTE_HIDDEN;
			SetFileAttributes(wpath.c_str(), attributes);
#endif
		}
		else
		{
			qDebug() << "temp dir could not be created :" << path;
		}
	}

	return path;
}
