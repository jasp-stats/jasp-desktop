#ifdef __APPLE__
//Keep all the macOS crap in this compilation unit please!

#include "macsandboxmanager.h"
#include "log.h"
#include "gui/preferencesmodel.h"

//Seatbelt profile for the JASP engine: everything is allowed except network. Two narrow
//exceptions are re-allowed so local machinery keeps working:
//  - unix domain sockets, so DNS (mDNSResponder) and local daemons stay reachable
//  - loopback TCP/UDP (connect, bind and listen), so socket-based R parallel backends
//    (parallel::makeCluster / PSOCK / snow) keep working
//
//Rule order matters, the last matching rule wins, so the allows must come after the deny.
//Filters may only be attached to the explicit network-outbound/network-bind/network-inbound
//operations: attaching them to the network* wildcard silently re-allows all network access
//(verified empirically on macOS 15.6, in all likelihood also the cause of bazelbuild/bazel#10068).
static const char* _engineNetworkProfile =
	"(version 1)"
	"(allow default)"
	"(deny network*)"
	"(allow network-outbound (remote unix-socket))"
	"(allow network-outbound (remote ip \"localhost:*\"))"
	"(allow network-bind network-inbound (local ip \"localhost:*\"))";

//Applying a seatbelt profile can fail (sandbox-exec exits with 71) when JASP itself already runs inside a
//sandbox that forbids it, for example on some CI-runners. Rather than having the engine die mystically over
//and over we probe once whether the actual engine profile can be applied and exec'd under, and simply start
//unsandboxed otherwise, mirroring the graceful-degradation the Windows sandbox has.
//NOTE: the probe must use the real profile: a bare "(version 1)" is deny-by-default and denies even the exec
//of the probe-command itself (learned the hard way: execvp EPERM, exit 71).
static bool seatbeltWorks()
{
	static bool works = []()
	{
		QProcess probe;
		probe.start(QStringLiteral("/usr/bin/sandbox-exec"), {QStringLiteral("-p"), QString::fromLatin1(_engineNetworkProfile), QStringLiteral("--"), QStringLiteral("/usr/bin/true")});
		if(!probe.waitForFinished(5000))
		{
			probe.kill();
			probe.waitForFinished(1000);
			Log::log() << "Seatbelt probe TIMED OUT, error: " << probe.errorString().toStdString() << std::endl;
			return false;
		}

		const bool ok = probe.exitStatus() == QProcess::NormalExit && probe.exitCode() == 0;
		if(ok)
			Log::log() << "Seatbelt probe OK, sandbox-exec works in this environment." << std::endl;
		else
		{
			const QByteArray err = probe.readAllStandardError();
			Log::log() << "Seatbelt probe FAILED: exitCode=" << probe.exitCode()
						<< " exitStatus=" << (probe.exitStatus() == QProcess::NormalExit ? "normal" : "crash")
						<< " error=" << probe.errorString().toStdString()
						<< " stderr=" << QString::fromUtf8(err).trimmed().toStdString() << std::endl;
		}

		return ok;
	}();

	return works;
}

bool MacSandboxManager::launchSandboxedEngine(QProcess* engineProcess, const QString& EngineExe, const QStringList& args)
{
	if(!PreferencesModel::prefs()->engineSandbox())
		return false;

	if(!seatbeltWorks())
	{
		Log::log() << "Cannot apply a seatbelt profile in this environment (is JASP itself sandboxed?), starting JASPEngine without network containment." << std::endl;
		return false;
	}

	QStringList sandboxArgs;
	sandboxArgs << QStringLiteral("-p") << QString::fromLatin1(_engineNetworkProfile) << EngineExe << args;

	engineProcess->start(QStringLiteral("/usr/bin/sandbox-exec"), sandboxArgs);

	Log::log() << "JASPEngine network containment set! (seatbelt: no external network, unix sockets and localhost stay available)" << std::endl;

	return true;
}

#endif
