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

bool checkIfAccessible(STARTUPINFOEX si, const std::vector<QDir>& paths)
{
	QDir programDir					= AppDirs::programDir();
	QString checkerExecutable		= programDir.absoluteFilePath("ContainerFilePermissionChecker");
	QProcess* checkProc				= new QProcess();
	QProcessEnvironment env			= QProcessEnvironment::systemEnvironment();
	ProcessHelper::fixPATHForWindows(env);
	checkProc->setProcessEnvironment(env);

	QStringList args;
	for(const QDir & path : paths)
		args << path.absolutePath();

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

	if(!checkIfAccessible(si, _fullAccessList)) {
		for(auto& dir : _fullAccessList) {
			Log::log() << "Attempting to grant access to: " << dir.absolutePath().toStdString() << std::endl;
			AllowNamedObjectAccess(appContainerSid, dir.absolutePath().toStdWString().data(), SE_FILE_OBJECT, FILE_ALL_ACCESS);
		}
	}

	//give access to exedir if needed
	if(!checkIfAccessible(si, {AppDirs::programDir().absolutePath()})) {
		QMessageBox* box = MessageForwarder::getInfoBox(QString("Intializing JASP security sandbox"), QString("Intializing JASP security sandbox"));
		box->show();
		grantAccessToExeDir();
		box->close();
	}

	//Show popup and disable the sandbox if it is really not working somehow
	if(!checkIfAccessible(si, {AppDirs::programDir().absolutePath()}) || !checkIfAccessible(si, {AppDirs::appData(false)})) {
        bool disable = MessageForwarder::showYesNo(QObject::tr("Security Sandbox Failure"), QObject::tr("Failed to activate Security Sandbox. Your system does not allow security sandboxing. Do you wish to continue with out is? (probably fine)"), QObject::tr("Continue"), QObject::tr("Exit"));
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
