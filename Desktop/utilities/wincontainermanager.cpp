#ifdef _WIN32
//Keep all the windows crap in this compilation unit please!

#include "wincontainermanager.h"
#include "userenv.h"
#include "log.h"
#include <atlsecurity.h>
#include "processhelper.h"
#include "utilities/appdirs.h"
#include "gui/preferencesmodel.h"
#include "utilities/messageforwarder.h"
#include "utilities/dynamicruntimeinfo.h"

std::wstring toWString(const std::string& in) {
	return std::wstring(in.begin(), in.end());
}

bool AllowNamedObjectAccess(PSID appContainerSid, PWSTR name, SE_OBJECT_TYPE type, ACCESS_MASK accessMask, DWORD inheritance = OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE) {
	PACL oldAcl, newAcl = nullptr;
	DWORD status;
	EXPLICIT_ACCESS access;
	do {
		access.grfAccessMode = GRANT_ACCESS;
		access.grfAccessPermissions = accessMask;
		access.grfInheritance = inheritance;
		access.Trustee.MultipleTrusteeOperation = NO_MULTIPLE_TRUSTEE;
		access.Trustee.pMultipleTrustee = nullptr;
		access.Trustee.ptstrName = (PWSTR)appContainerSid;
		access.Trustee.TrusteeForm = TRUSTEE_IS_SID;
		access.Trustee.TrusteeType = TRUSTEE_IS_GROUP;

		status = GetNamedSecurityInfo(name, type, DACL_SECURITY_INFORMATION, nullptr, nullptr, &oldAcl, nullptr, nullptr);
		if (status != ERROR_SUCCESS)
			return false;

		status = SetEntriesInAcl(1, &access, oldAcl, &newAcl);
		if (status != ERROR_SUCCESS)
			return false;

		status = SetNamedSecurityInfo(name, type, DACL_SECURITY_INFORMATION, nullptr, nullptr, newAcl, nullptr);
		if (status != ERROR_SUCCESS)
			break;
	} while (false);

	if (newAcl)
		::LocalFree(newAcl);

	return status == ERROR_SUCCESS;
}

bool grantAccessToExeDir() {

	auto grantStandardRead = [](std::wstring path, std::wstring group) {
	PACL oldAcl, newAcl = nullptr;
	DWORD status;
	EXPLICIT_ACCESS access;
	do {
			access.grfAccessMode = GRANT_ACCESS;
			access.grfAccessPermissions = GENERIC_EXECUTE | GENERIC_READ;
			access.grfInheritance = OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE;
			access.Trustee.MultipleTrusteeOperation = NO_MULTIPLE_TRUSTEE;
			access.Trustee.pMultipleTrustee = nullptr;
			access.Trustee.TrusteeForm = TRUSTEE_IS_NAME;
			access.Trustee.TrusteeType = TRUSTEE_IS_WELL_KNOWN_GROUP;
			access.Trustee.ptstrName = group.data();

			status = GetNamedSecurityInfo(path.c_str(), SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nullptr, nullptr, &oldAcl, nullptr, nullptr);
			if (status != ERROR_SUCCESS)
				return false;

			status = SetEntriesInAcl(1, &access, oldAcl, &newAcl);
			if (status != ERROR_SUCCESS)
				return false;

			status = SetNamedSecurityInfo(path.data(), SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nullptr, nullptr, newAcl, nullptr);
			if (status != ERROR_SUCCESS)
				break;
		} while (false);

		if (newAcl)
			::LocalFree(newAcl);

		return status == ERROR_SUCCESS;
	};

	std::wstring exedir = AppDirs::programDir().absolutePath().toStdWString();
	bool res = grantStandardRead(exedir, L"ALL APPLICATION PACKAGES");
	res &= grantStandardRead(exedir, L"ALL RESTRICTED APP PACKAGES");
	return res;
}

enum class ProbeMode { ReadOnly, ReadWrite };

//Decodes the packed exit code of ContainerFilePermissionChecker (stage<<24 | pathIndex<<16 | winError)
//into something readable for the log. Stage ids must match the checker source.
static QString describeProbeFailure(int exitCode, const std::vector<QDir>& paths)
{
	static const char* stageNames[] = {
		"",
		"enumerate directory",
		"open existing file",
		"read existing file",
		"create probe file",
		"write probe file",
		"reopen probe file",
		"read back probe file",
		"delete probe file"
	};

	if (exitCode == -1)	return QStringLiteral("checker usage error (wrong arguments)");

	const int	stage		= (exitCode >> 24) & 0xFF,
				pathIndex	= (exitCode >> 16) & 0xFF,
				winError	= exitCode & 0xFFFF;
	const QString			path		= pathIndex >= 0 && pathIndex < static_cast<int>(paths.size()) ? paths[pathIndex].absolutePath() : QStringLiteral("?");
	const QString			stageName	= stage > 0 && stage < 9 ? QString::fromLatin1(stageNames[stage]) : QStringLiteral("unknown stage");

	return QStringLiteral("stage='%1' path='%2' winError=%3").arg(stageName, path).arg(winError);
}

bool checkIfAccessible(STARTUPINFOEX si, const std::vector<QDir>& paths, ProbeMode mode)
{
	QDir programDir					= AppDirs::programDir();
	QString checkerExecutable	= programDir.absoluteFilePath("ContainerFilePermissionChecker");
	QProcess checkProc;
	QProcessEnvironment env		= QProcessEnvironment::systemEnvironment();
	ProcessHelper::fixPATHForWindows(env);
	checkProc.setProcessEnvironment(env);

	QStringList args;
	args << (mode == ProbeMode::ReadWrite ? "-rw" : "-r");
	for(const QDir & path : paths)
		args << path.absolutePath();

	checkProc.setCreateProcessArgumentsModifier([si] (QProcess::CreateProcessArguments *args)
	{
		args->inheritHandles = false;
		args->flags = args->flags | EXTENDED_STARTUPINFO_PRESENT;
		args->startupInfo = (LPSTARTUPINFO)&si;
	});

	checkProc.start(checkerExecutable, args);

	//A checker that has not finished in time is a failure, not a pass: exitCode() defaults to 0 while running.
	if (!checkProc.waitForFinished(5000))
	{
		checkProc.kill();
		checkProc.waitForFinished(1000);
		Log::log() << "Container is currently missing file permission: permission checker timed out on " << args.join(QStringLiteral(", ")).toStdString() << std::endl;
		return false;
	}

	if (checkProc.exitStatus() != QProcess::NormalExit)
	{
		Log::log() << "Container is currently missing file permission: permission checker crashed (exit code " << checkProc.exitCode() << ")" << std::endl;
		return false;
	}

	const int exitCode = checkProc.exitCode();
	if (exitCode != 0)
		Log::log() << "Container is currently missing file permission (" << describeProbeFailure(exitCode, paths).toStdString() << "), we will have to grant them" << std::endl;

	return exitCode == 0;
}


bool WinContainerManager::launchSandboxedEngine(QProcess* engineProcess, const QString& EngineExe, const QStringList& args)
{
	if(!PreferencesModel::prefs()->engineSandbox())
		return false;

	std::wstring containerName = toWString(_containerName);
	PSID appContainerSid;
	auto hr = ::CreateAppContainerProfile(containerName.c_str(), containerName.c_str(), containerName.c_str(), nullptr, 0, &appContainerSid);
	if (FAILED(hr)) {
		// see if AppContainer SID already exists
		hr = ::DeriveAppContainerSidFromAppContainerName(containerName.c_str(), &appContainerSid);
		if (FAILED(hr))
			throw std::runtime_error("Could not get a appcontainer PSID");
	}

	//create startup info for processes using the container
	SECURITY_CAPABILITIES sc = { 0 };
	sc.AppContainerSid = appContainerSid;

	STARTUPINFOEX si = { sizeof(si) };
	SIZE_T size;

	::InitializeProcThreadAttributeList(nullptr, 1, 0, &size);
	auto buffer = std::make_unique<BYTE[]>(size);
	si.lpAttributeList = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(buffer.get());
	if (!::InitializeProcThreadAttributeList(si.lpAttributeList, 1, 0, &size))
		throw std::runtime_error("Failure initializing appcontainer attr list");
	if (!::UpdateProcThreadAttribute(si.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_SECURITY_CAPABILITIES, &sc, sizeof(sc), nullptr, nullptr))
		throw std::runtime_error("Failure setting appcontainer attr list");


	//handle the file permissions of the container
	std::vector<QDir> _fullAccessList = {
		QString(Dirs::tempDir().c_str()),
		AppDirs::appData(false), //entire appdata dir, might want to give more fine grained access when R pkgs are installed here
		AppDirs::appData(), //logdir
		AppDirs::sandboxedDocuments(),
		AppDirs::userModulesDir()
	};

	//Are we running from a buildfolder? Because then we have no access to qt dlls yet, cause theyre not in the buildfolder yet.
	if(DynamicRuntimeInfo::getInstance()->getRuntimeEnvironment() == RuntimeEnvironment::UNKNOWN)
	{
		static auto env = QProcessEnvironment::systemEnvironment();
		if(env.value("QTDIR") != "")
			_fullAccessList.push_back(env.value("QTDIR") + "/bin");
	}

	//EFS self-report: ACL grants cannot fix EFS (it is keys, not access lists), so if any path the engine needs is
	//EFS-encrypted the engine will fail there no matter what we grant. Log it loudly at every launch: this is the
	//telemetry that decides between the hypotheses in the Windows sandbox/EFS analysis (jasp-issues#4566, #3562).
	//Unencrypted paths stay silent to keep the log readable across engine restarts.
	{
		std::vector<QDir> efsCheckPaths = _fullAccessList;
		efsCheckPaths.push_back(AppDirs::programDir());
		for (const QDir & dir : efsCheckPaths)
		{
			const DWORD attributes = GetFileAttributesW(dir.absolutePath().toStdWString().c_str());
			if (attributes == INVALID_FILE_ATTRIBUTES)
				Log::log() << "EFS check: could not read attributes of '" << dir.absolutePath().toStdString() << "'" << std::endl;
			else if (attributes & FILE_ATTRIBUTE_ENCRYPTED)
				Log::log() << "EFS check: '" << dir.absolutePath().toStdString() << "' is EFS-ENCRYPTED, file permissions cannot fix this and the engine will likely fail there!" << std::endl;
		}
	}

	if(!checkIfAccessible(si, _fullAccessList, ProbeMode::ReadWrite)) {
		for(auto& dir : _fullAccessList) {
			Log::log() << "Attempting to grant access to: " << dir.absolutePath().toStdString() << std::endl;
			AllowNamedObjectAccess(appContainerSid, dir.absolutePath().toStdWString().data(), SE_FILE_OBJECT, FILE_ALL_ACCESS);
		}
	}

	//give access to exedir if needed
	if(!checkIfAccessible(si, {AppDirs::programDir().absolutePath()}, ProbeMode::ReadOnly)) {
		QMessageBox* box = MessageForwarder::getInfoBox(QString("Intializing JASP security sandbox"), QString("Intializing JASP security sandbox"));
		box->show();
		grantAccessToExeDir();
		box->close();
	}

	//Show popup and disable the sandbox if it is really not working somehow
	if(!checkIfAccessible(si, {AppDirs::programDir().absolutePath()}, ProbeMode::ReadOnly) || !checkIfAccessible(si, {AppDirs::appData(false)}, ProbeMode::ReadWrite)) {
		bool disable = MessageForwarder::showYesNo(QObject::tr("Security Sandbox Failure"), QObject::tr("Failed to activate Security Sandbox. Your system does not allow security sandboxing. Do you wish to continue without it? (probably fine)"), QObject::tr("Continue"), QObject::tr("Exit"));
		if(disable) {
			Log::log() << "Disabling Sandbox" << std::endl;
			PreferencesModel::prefs()->setEngineSandbox(false);
			return false;
		}
		else {
			Log::log() << "Sandbox Failure, User selected exit" << std::endl;
			exit(13);
		}
	}

	//set the startup info for the engine
	engineProcess->setCreateProcessArgumentsModifier([si] (QProcess::CreateProcessArguments *args)
	{
		args->inheritHandles = false;
		args->flags = args->flags | EXTENDED_STARTUPINFO_PRESENT;
		args->startupInfo = (LPSTARTUPINFO)&si;
	});

	engineProcess->start(EngineExe, args);

	Log::log() << "JASPEngine containment set!" << std::endl;

	return true;
}

#endif
