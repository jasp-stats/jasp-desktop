#ifdef __APPLE__

#ifndef MAC_SANDBOX_MAN_H
#define MAC_SANDBOX_MAN_H

#include <QProcess>
#include <QString>
#include <QStringList>

class MacSandboxManager
{

public: //NOTE lets not leak macos crap out of this class!!!!

	//Starts the engine wrapped in /usr/bin/sandbox-exec with a Seatbelt profile that denies
	//all network access except unix domain sockets and loopback. Returns false (and starts
	//nothing) when the engine sandbox is disabled, so the caller can start it normally.
	static bool launchSandboxedEngine(QProcess* engineProcess, const QString& EngineExe, const QStringList& args);

private:
	MacSandboxManager();
};

#endif // MAC_SANDBOX_MAN_H

#endif
