
#include "appdirs.h"

#include <QApplication>
#include <QDir>
#include <QDebug>

#ifdef __WIN32__
#include <windows.h>
#include "qutils.h"
#include "utils.h"
#endif

#include "dirs.h"

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

const QString AppDirs::analysisDefaultsDir()
{
	QString path = QString::fromStdString(Dirs::appDataDir()) + "/AnalysisDefaults";
	QDir dir(path);
	dir.mkpath(".");

	return path;
}
