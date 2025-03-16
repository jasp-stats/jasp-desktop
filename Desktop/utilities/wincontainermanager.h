#ifdef _WIN32

#ifndef WINCONTAINER_MAN_H
#define WINCONTAINER_MAN_H

#include <string>
#include <QProcess>
#include "dirs.h"
#include "utilities/appdirs.h"

class WinContainerManager
{

public: //NOTE lets not leak windows crap out of this class!!!!

	static bool launchSandboxedEngine(QProcess* engineProcess, const QString& EngineExe, const QStringList& args);

private:
	WinContainerManager();

	inline static const std::string _containerName = "_JASP_JASPENGINE_V1";
};

#endif // WINCONTAINER_MAN_H

#endif
