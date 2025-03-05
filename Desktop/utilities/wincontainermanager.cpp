#ifdef _WIN32
//Keep all the windows crap in this compilation unit please!

#include "wincontainermanager.h"
#include "userenv.h"
#include <atlsecurity.h>
#include "log.h"

WinContainerManager* WinContainerManager::_instance = nullptr;
WinContainerManager::Destroyer* WinContainerManager::_destroyer = nullptr;

class WinContainerManager::Container {
public:
	Container(): permissions(Permissions::NONE){};
	Container(const std::string& _name, Permissions _permissions);
	Container(const std::string& _name, Permissions _permissions, const std::vector<std::filesystem::path>& _dirs, const std::vector<std::filesystem::path>& _files);

	// bool launchExec(std::filesystem::path execPath);

	std::wstring name;
	Permissions permissions;
	std::vector<std::filesystem::path> accessible_dirs;
	std::vector<std::filesystem::path> accessible_files;

	STARTUPINFOEX startup_info = { sizeof(startup_info) };


	//Can extend with running execs etc etc

private:
	PSID psid;
	unsigned char* attr_buf;
	unsigned char* sidBuffer;

	SECURITY_CAPABILITIES sc = { 0  };
	SID_AND_ATTRIBUTES cap;

};


WinContainerManager* WinContainerManager::getInstance() {
	if(_instance)
		return _instance;
	_instance = new WinContainerManager();
	_destroyer = new WinContainerManager::Destroyer();
	_destroyer->_singleton= _instance;
	return _instance;
}

WinContainerManager::~WinContainerManager()
{
	for(auto& nameContainerPair : nameToContainer) {
		delete nameContainerPair.second;
	}
}



//with help from https://scorpiosoftware.net/2019/01/15/fun-with-appcontainers/      https://github.com/zodiacon/RunAppContainer/blob/master/RunAppContainer/RunAppContainerDlg.cpp
WinContainerManager::Container::Container(const std::string& _name, Permissions _permissions, const std::vector<std::filesystem::path>& _dirs, const std::vector<std::filesystem::path>& _files) : name(_name.begin(), _name.end()), permissions(_permissions), accessible_dirs(_dirs), accessible_files(_files) {
	Log::log() << "!!!hi there" << std::endl;
    auto hr = CreateAppContainerProfile(name.c_str() , name.c_str(), name.c_str(), nullptr, 0, &psid);
    if (FAILED(hr)) {
        // see if AppContainer SID already exists
        hr = DeriveAppContainerSidFromAppContainerName(name.c_str(), &psid);
        if (FAILED(hr))
            throw std::exception("Error creating container profile!");
    }

	sc = { 0 };
    sc.AppContainerSid = psid;

	// sidBuffer = new unsigned char[SECURITY_MAX_SID_SIZE];
	// PSID allCapability = reinterpret_cast<PSID>(sidBuffer);
	// DWORD sizeSid;
	// if (!CreateWellKnownSid(WinCreatorGroupSid, nullptr, allCapability, &sizeSid))
	// 	throw std::exception("Error creating SID");
	// cap.Sid = allCapability;
	// cap.Attributes = SE_GROUP_ENABLED;

	// sc.Capabilities = &cap;
	// sc.CapabilityCount = 1;

    SIZE_T size;
    InitializeProcThreadAttributeList(nullptr, 1, 0, &size);
	attr_buf = new unsigned char[size];
	startup_info.lpAttributeList = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(attr_buf);
    if (!InitializeProcThreadAttributeList(startup_info.lpAttributeList, 1, 0, &size))
        throw std::exception("Error initing attribute list");
    if (!UpdateProcThreadAttribute(startup_info.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_SECURITY_CAPABILITIES, &sc, sizeof(sc), nullptr, nullptr))
        throw std::exception("Error setting attribute list");


	// PACL oldAcl, newAcl = nullptr;
	// DWORD status;
	// EXPLICIT_ACCESS access;
	// access.grfAccessMode = GRANT_ACCESS;
	// access.grfAccessPermissions = FILE_ALL_ACCESS;
	// access.grfInheritance = OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE;
	// access.Trustee.MultipleTrusteeOperation = NO_MULTIPLE_TRUSTEE;
	// access.Trustee.pMultipleTrustee = nullptr;
	// access.Trustee.ptstrName = (PWSTR)psid;
	// access.Trustee.TrusteeForm = TRUSTEE_IS_SID;
	// access.Trustee.TrusteeType = TRUSTEE_IS_GROUP;

	// std::string x= "C:\\Users\\rdoff\\Documents\\abc.txt";
	// std::wstring tmp = std::wstring(x.begin(), x.end());
	// status = GetNamedSecurityInfo(tmp.data(), SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nullptr, nullptr, &oldAcl, nullptr, nullptr);
	// if (status != ERROR_SUCCESS)
	// 	throw std::exception("Couldnt get ACL");

	// status = SetEntriesInAcl(1, &access, oldAcl, &newAcl);
	// if (status != ERROR_SUCCESS)
	// 	throw std::exception("Couldnt alter ACL");

	// status = SetNamedSecurityInfo(tmp.data(), SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nullptr, nullptr, newAcl, nullptr);
	// if (status != ERROR_SUCCESS)
	// 	throw std::exception("Couldnt set ACL");;



		PROCESS_INFORMATION pi;
	std::string execPath = "C:\\Users\\rdoff\\source\\repos\\Project1\\Project1\\x64\\Debug\\Project1.exe";
		bool created = CreateProcess(nullptr, std::wstring(execPath.begin(), execPath.end()).data(), nullptr, nullptr, FALSE,
									   EXTENDED_STARTUPINFO_PRESENT, nullptr, nullptr, (LPSTARTUPINFO)&startup_info, &pi);

	// 	// std::string execPath = "C:\\WINDOWS\\notepad.exe";
	// 	// bool created = CreateProcess(nullptr, std::wstring(execPath.begin(), execPath.end()).data(), nullptr, nullptr, FALSE,
	// 	// 							 0, nullptr, nullptr, nullptr, &pi);



	// 	WaitForSingleObject( pi.hProcess, INFINITE );
		return;
    // set security for files/folders

    // int start = 0;
    // do {
    //     auto filename = files.Tokenize(L"\r\n", start);
    //     if (filename.IsEmpty())
    //         break;
    //     AllowNamedObjectAccess(appContainerSid, filename.GetBuffer(), SE_FILE_OBJECT, FILE_ALL_ACCESS);
    // } while (true);

    // start = 0;
    // do {
    //     auto filename = registry.Tokenize(L"\r\n", start);
    //     if (filename.IsEmpty())
    //         break;
    //     AllowNamedObjectAccess(appContainerSid, filename.GetBuffer(), SE_REGISTRY_WOW64_32KEY, KEY_ALL_ACCESS);
    // } while (true);
}

// bool WinContainerManager::Container::launchExec(std::filesystem::path _execPath) {

//     std::wstring execPath = _execPath.wstring();
//     PROCESS_INFORMATION pi;

//     bool created = CreateProcess(nullptr, execPath.data(), nullptr, nullptr, FALSE,
//                                    EXTENDED_STARTUPINFO_PRESENT, nullptr, nullptr, (LPSTARTUPINFO)&startup_info, &pi);

//     return created;
// }



bool WinContainerManager::createNewContainer(const std::string& name, Permissions permissions, const std::vector<std::filesystem::path> &accessibleDirs, const std::vector<std::filesystem::path> &accessibleFiles)
{
    try {
		nameToContainer.insert({name, new Container(name, permissions, {}, {})});
    } catch (std::exception& ex) {
        Log::log() << ex.what() << std::endl;
        return false;
    }
    return true;
}

bool WinContainerManager::getWin32StartupArgs(const std::string &containerName, win32StartupArgs& args)
{
	if(!containerExists(containerName)) {
		Log::log() << "container: " << containerName << "does not exist!" << std::endl;
		return false;
	}

	args.flags = EXTENDED_STARTUPINFO_PRESENT;
	args.startupInfo = &nameToContainer[containerName]->startup_info;
	return true;
}


bool WinContainerManager::containerExists(const std::string &containerName)
{
	return nameToContainer.find(containerName) != nameToContainer.end();
}
// bool WinContainerManager::launchExecInContainer(const std::string containerName, const std::filesystem::path pathToExec)
// {

//     if(nameToContainer.find(containerName) == nameToContainer.end()) {
//         Log::log() << "Container with name: " << containerName << "does not exist!" << std::endl;
//         return false;
//     }

//     Container& c = nameToContainer[containerName];
//     return c.launchExec(pathToExec);
// }





#endif

