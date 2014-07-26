
#include "appdirs.h"

#include <QApplication>
#include <QDir>

const QString &AppDirs::examples()
{
#ifdef __WIN32__
	static QString dir = QApplication::applicationDirPath() + QDir::separator() + "Data Sets";
#else
	static QString dir = QApplication::applicationDirPath() + "/../Resources/Data Sets";
#endif

	return dir;
}
