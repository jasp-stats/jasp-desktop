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

	inline static const bool createJASPDownloadFolder = false;
	inline static const std::string downloadFolderName = "JASP_SAFE_IO";


};

#endif // WINCONTAINER_MAN_H

#endif
