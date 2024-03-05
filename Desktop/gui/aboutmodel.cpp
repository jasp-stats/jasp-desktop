#include "aboutmodel.h"
#include "appinfo.h"
#include "utilities/qutils.h"
#include "mainwindow.h"

bool AboutModel::visible() const
{
	return _visible;
}

QString AboutModel::version()
{
	return MainWindow::versionString();
}

QString AboutModel::buildDate()
{
	return tq(AppInfo::builddate);
}

QString AboutModel::copyrightMessage()
{
	return "Copyright 2013-" + tq(AppInfo::getBuildYear()) + " University of Amsterdam";
}

QString AboutModel::citation()
{
	return tq("JASP Team (" +  AppInfo::getBuildYear() + ").\nJASP (Version " + AppInfo::version.asString() //+
	//ok nvm the debug stuff I just want it to look like it does in release
	//	#ifdef JASP_DEBUG
	//		"-Debug-" + AppInfo::gitCommit //If it is a debug version that is actually being cited (highly unusual I suppose and downright dangerous, but ok.) In that scenario I'm pretty sure one would want to know the exact commit even though it might be ugly.
	//	#endif
			+ ") [Computer software].");
}

QString AboutModel::commit()
{
	return tq(AppInfo::gitCommit);
}

QString AboutModel::branch()
{
	return tq(AppInfo::gitBranch);
}

void AboutModel::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}

QString AboutModel::systemInfo()
{
    QString info;
    QProcess process;
    QString command;
    QStringList arguments;

#ifdef _WIN32
    command = "powershell";
    // so Windows use local code page and we change it to 65001 for print(will not available for global)
    arguments << "-NoProfile" << "-Command" << "Write-Host \"Current code page\"; chcp ; chcp 65001 ; systeminfo";
#elif defined(__APPLE__)
    command = "system_profiler";
    arguments << "SPSoftwareDataType SPHardwareDataType";
#elif defined(__linux__)
    command = "bash";
    arguments << "-c" << "lshw -short";
#endif

    if(!AboutModel().visible())
    {
        info += "-------- Basic Info --------\n";
        info += "Operating System: " + QSysInfo::prettyProductName() + "\n";
        info += "Product Version: "  + QSysInfo::productVersion() + "\n";
        info += "Kernel Type: "      + QSysInfo::kernelType() + "\n";
        info += "Kernel Version: "   + QSysInfo::kernelVersion() + "\n";
        info += "Architecture: "     + QSysInfo::currentCpuArchitecture() + "\n";
        info += "Install Path: "     + QCoreApplication::applicationDirPath() + "\n";
        info += "System Local: "     + QLocale::system().name() + "\n\n";

        process.start(command, arguments);
        process.waitForFinished(5000);

        QByteArray outputData = process.readAllStandardOutput();
        process.close();

		if(outputData.trimmed().length())
			info += "-------- Extra Info --------\n" + outputData + "\n";
    }

    return info;
}
