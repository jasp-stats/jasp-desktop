#ifdef _WIN32
//Keep all the windows crap in this compilation unit please!

#include "wincontainermanager.h"
#include "userenv.h"
#include <atlsecurity.h>
#include "log.h"
#include "utilities/appdirs.h"

std::wstring toWString(const std::string& in) {
	return std::wstring(in.begin(), in.end());
}


bool AllowNamedObjectAccess(PSID appContainerSid, PWSTR name, SE_OBJECT_TYPE type, ACCESS_MASK accessMask) {
	PACL oldAcl, newAcl = nullptr;
	DWORD status;
	EXPLICIT_ACCESS access;
	do {
		access.grfAccessMode = GRANT_ACCESS;
		access.grfAccessPermissions = accessMask;
		access.grfInheritance = OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE;
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

bool checkIfAccessible(STARTUPINFOEX si, const std::vector<std::string>& paths)
{
	QDir programDir					= AppDirs::programDir();
	QString checkerExecutable		= programDir.absoluteFilePath("ContainerFilePermissionChecker");
	QProcess* checkProc = new QProcess();

	QStringList args;
	for(const std::string& path : paths)
		args << QString(path.c_str());

	checkProc->setCreateProcessArgumentsModifier([si] (QProcess::CreateProcessArguments *args)
	{
		args->inheritHandles = false;
		args->flags = args->flags | EXTENDED_STARTUPINFO_PRESENT;
		args->startupInfo = (LPSTARTUPINFO)&si;
	});

	checkProc->start(checkerExecutable, args);
	checkProc->waitForFinished(1000);
	int result = checkProc->exitCode() == 0;
	if(!result)
		Log::log() << "Container is currently missing file permisson, we will have to grant them" << std::endl;
	return result;
}


bool WinContainerManager::launchSandboxedEngine(QProcess* engineProcess, const QString& EngineExe, const QStringList& args)
{
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
	const std::vector<std::string> _fullAccessList = {
		Dirs::tempDir(),
		AppDirs::appData(false).toStdString(), //entire appdata dir, might want to give more fine grained access when R pkgs are installed here
		AppDirs::appData().toStdString() //logdir
	};

	if(!checkIfAccessible(si, _fullAccessList)) {
		for(auto& file : _fullAccessList)
			AllowNamedObjectAccess(appContainerSid, toWString(file).data(), SE_FILE_OBJECT, FILE_ALL_ACCESS);
	}

	const std::vector<std::string> _readExecuteList = {
		AppDirs::programDir().filesystemAbsolutePath().string() //we can make this finer
	};

	if(!checkIfAccessible(si, _readExecuteList)) {
		for(auto& file : _readExecuteList)
			AllowNamedObjectAccess(appContainerSid, toWString(file).data(), SE_FILE_OBJECT, FILE_ALL_ACCESS); //FILE_EXECUTE | FILE_READ_DATA | FILE_READ_ATTRIBUTES | FILE_LIST_DIRECTORY);
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
