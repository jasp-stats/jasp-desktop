//EFSPoC launcher: the full-trust (packagedClassicApp, mediumIL) entry point, standing in for JASPDesktop.
//It derives this package's family AppContainer SID (API + manual SHA-256 fallback), reports the EFS state of
//the package LocalCache (the Phase 3 gate question), writes a test file there, and then spawns EFSPoCEngine.exe
//in an AppContainer under that SID - exactly what Phase 3 wants JASP to do with JASPEngine.
//Everything lands in %LOCALAPPDATA%\Packages\<family>\LocalCache\poc-launcher.log.

#include <windows.h>
#include <appmodel.h>
#include <userenv.h>
#include <bcrypt.h>
#include <sddl.h>
#include <string>
#include <vector>
#include <cstdio>

#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "userenv.lib")
#pragma comment(lib, "bcrypt.lib")

namespace
{
	std::wstring g_logPath;

	std::string narrow(const std::wstring& wide)
	{
		return std::string(wide.begin(), wide.end());
	}

	void log(const std::string& line)
	{
		HANDLE file = CreateFileW(g_logPath.c_str(), FILE_APPEND_DATA, FILE_SHARE_READ, nullptr, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
		if (file == INVALID_HANDLE_VALUE) return;
		DWORD written = 0;
		WriteFile(file, line.c_str(), static_cast<DWORD>(line.size()), &written, nullptr);
		WriteFile(file, "\r\n", 2, &written, nullptr);
		CloseHandle(file);
	}

	std::wstring sidToString(PSID sid)
	{
		LPWSTR stringSid = nullptr;
		if (!ConvertSidToStringSidW(sid, &stringSid))
			return L"(sid-to-string-failed)";
		std::wstring result(stringSid);
		LocalFree(stringSid);
		return result;
	}

	std::wstring getFamilyName()
	{
		WCHAR familyName[PACKAGE_FAMILY_NAME_MAX_LENGTH + 1] = {};
		UINT32 length = ARRAYSIZE(familyName);
		if (GetCurrentPackageFamilyName(&length, familyName) != ERROR_SUCCESS)
			return L"";
		return familyName;
	}

	std::wstring getLocalCacheDir()
	{
		WCHAR localAppData[MAX_PATH] = {};
		GetEnvironmentVariableW(L"LOCALAPPDATA", localAppData, MAX_PATH);
		return std::wstring(localAppData) + L"\\Packages\\" + getFamilyName() + L"\\LocalCache";
	}

	void ensureDirectory(const std::wstring& dir)
	{
		for (size_t pos = dir.find(L'\\'); pos != std::wstring::npos; pos = dir.find(L'\\', pos + 1))
			CreateDirectoryW(dir.substr(0, pos).c_str(), nullptr);
		CreateDirectoryW(dir.c_str(), nullptr);
	}

	//Manual derivation, per Raymond Chen (2022): S-1-15-2-<first 28 bytes of SHA-256 over the
	//downcased UTF-16 moniker, read as 7 big-endian DWORDs>. Needed because WindowsAppSDK#787:
	//DeriveAppContainerSidFromAppContainerName returns success-but-null for the caller's own family.
	PSID manuallyDeriveAppContainerSid(const std::wstring& familyName)
	{
		std::wstring lower;
		lower.reserve(familyName.size());
		for (wchar_t c : familyName)
			lower.push_back(static_cast<wchar_t>(towlower(c)));

		BCRYPT_ALG_HANDLE algorithm = nullptr;
		BCRYPT_HASH_HANDLE hash = nullptr;
		BYTE sha256[32] = {};
		bool ok =	BCryptOpenAlgorithmProvider(&algorithm, BCRYPT_SHA256_ALGORITHM, nullptr, 0) == 0
				&&	BCryptCreateHash(algorithm, &hash, nullptr, 0, nullptr, 0, 0) == 0
				&&	BCryptHashData(hash, reinterpret_cast<PUCHAR>(&lower[0]), static_cast<ULONG>(lower.size() * sizeof(wchar_t)), 0) == 0
				&&	BCryptFinishHash(hash, sha256, sizeof(sha256), 0) == 0;
		if (hash)		BCryptDestroyHash(hash);
		if (algorithm)	BCryptCloseAlgorithmProvider(algorithm, 0);
		if (!ok) return nullptr;

		std::wstring sidString = L"S-1-15-2";
		for (int i = 0; i < 7; i++)
		{
			//Validated against the OS: sub-authorities are the first 28 hash bytes read as LITTLE-endian DWORDs
			//(ground truth: the OS-recorded SID of JASP's _JASP_JASPENGINE_V1 container, 2026-09-30).
			const DWORD subAuthority =	static_cast<DWORD>(sha256[i * 4 + 3])	<< 24
										|	static_cast<DWORD>(sha256[i * 4 + 2])	<< 16
										|	static_cast<DWORD>(sha256[i * 4 + 1])	<< 8
										|	static_cast<DWORD>(sha256[i * 4]);
			sidString += L"-" + std::to_wstring(subAuthority);
		}

		PSID sid = nullptr;
		ConvertStringSidToSidW(sidString.c_str(), &sid);
		return sid;
	}
}

int WINAPI wWinMain(_In_ HINSTANCE, _In_opt_ HINSTANCE, _In_ PWSTR, _In_ int)
{
	const std::wstring familyName = getFamilyName();
	if (familyName.empty())
	{
		MessageBoxW(nullptr, L"Not running with package identity - install the msix and launch it from the Start menu.", L"EFSPoC", MB_ICONERROR);
		return 1;
	}

	ensureDirectory(getLocalCacheDir());
	g_logPath = getLocalCacheDir() + L"\\poc-launcher.log";
	log("================ EFSPoC launcher start ================");
	log("Package family name: " + narrow(familyName));

	//Derive the package-family AppContainer SID: API first, manual hash as fallback, and log whether they agree.
	PSID appContainerSid = nullptr;
	HRESULT hr = DeriveAppContainerSidFromAppContainerName(familyName.c_str(), &appContainerSid);
	log("DeriveAppContainerSidFromAppContainerName hr=0x" + std::to_string(static_cast<unsigned long>(hr)) + " sid=" +
		(appContainerSid ? narrow(sidToString(appContainerSid)) : "(null - WindowsAppSDK#787 fallback engaged)"));

	PSID manualSid = manuallyDeriveAppContainerSid(familyName);
	if (manualSid)
		log("Manual SHA-256 derivation:                          sid=" + narrow(sidToString(manualSid)));
	if (appContainerSid && manualSid)
		log(std::string("API and manual derivation agree: ") + (EqualSid(appContainerSid, manualSid) ? "YES" : "NO - fallback formula is wrong!"));

	//Self-validation of the fallback formula: for a moniker that is NOT our own family the API is supposed to
	//work, so both paths can be compared. JASP's engine container is a perfect fixed reference point - its true
	//SID is also stamped as an ACE on JASP's granted temp dir, so any deviation is the API acting up in a
	//packaged process, not the formula.
	{
		PSID apiSid = nullptr, formulaSid = nullptr;
		if (SUCCEEDED(DeriveAppContainerSidFromAppContainerName(L"_JASP_JASPENGINE_V1", &apiSid)) && apiSid)
		{
			formulaSid = manuallyDeriveAppContainerSid(L"_JASP_JASPENGINE_V1");
			log("Self-test reference API SID for _JASP_JASPENGINE_V1:  " + narrow(sidToString(apiSid)));
			log("Self-test formula SID for _JASP_JASPENGINE_V1:        " + (formulaSid ? narrow(sidToString(formulaSid)) : "(null)"));
			log(std::string("Self-test verdict (OS-recorded truth is S-1-15-2-2341182285-1701709768-75279733-4070463477-2177057254-439173099-1957538822): ")
				+ (formulaSid && EqualSid(apiSid, formulaSid) ? "formula == API" : "see strings above - formula is the ground truth if it shows the 2341182285-... SID"));
		}
		if (apiSid) FreeSid(apiSid);
		if (formulaSid) LocalFree(formulaSid);
	}

	if (!appContainerSid)
		appContainerSid = manualSid;
	if (!appContainerSid)
	{
		log("FATAL: could not derive any AppContainer SID");
		return 2;
	}

	//The Phase 3 gate question: does this machine EFS-encrypt our package LocalCache?
	const DWORD attributes = GetFileAttributesW(getLocalCacheDir().c_str());
	log("LocalCache attributes: 0x" + std::to_string(attributes) +
		((attributes & FILE_ATTRIBUTE_ENCRYPTED) ? "  <-- EFS-ENCRYPTED (Application Protected family)" : "  (not encrypted on this machine)"));

	//Write a test file (full-trust side) for the engine to read back.
	const std::wstring testFile = getLocalCacheDir() + L"\\launcher-file.txt";
	HANDLE file = CreateFileW(testFile.c_str(), GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
	if (file == INVALID_HANDLE_VALUE)
		log("Launcher create test file: FAILED err=" + std::to_string(GetLastError()));
	else
	{
		const char data[] = "efspoc";
		DWORD written = 0;
		WriteFile(file, data, sizeof(data), &written, nullptr);
		CloseHandle(file);
		log("Launcher create test file: OK");
	}

	//A/B EXPERIMENT against the encrypted package: spawn the engine twice -
	//  1) under JASP's CURRENT custom container (_JASP_JASPENGINE_V1): "today's JASP", expected to FAIL EFS reads
	//  2) under the package-family SID (Phase 3): expected to pass
	WCHAR modulePath[MAX_PATH] = {};
	GetModuleFileNameW(nullptr, modulePath, MAX_PATH);
	std::wstring exe(modulePath);
	exe.replace(exe.find_last_of(L'\\') + 1, std::wstring::npos, L"EFSPoCEngine.exe");
	log("Engine exe path: " + narrow(exe));

	PSID customSid = manuallyDeriveAppContainerSid(L"_JASP_JASPENGINE_V1");
	log("Custom container SID (_JASP_JASPENGINE_V1, = today's JASP): " + (customSid ? narrow(sidToString(customSid)) : "(null)"));

	//Ensure an AppContainer profile is registered for the family: spawns under an unregistered SID fail with
	// err=2. Benign if it already exists. (The SID this returns inside a packaged process is a CHILD SID - we
	//never use it, only the profile's existence matters.)
	{
		PSID ensureSid = nullptr;
		const HRESULT profileHr = CreateAppContainerProfile(familyName.c_str(), familyName.c_str(), familyName.c_str(), nullptr, 0, &ensureSid);
		log("CreateAppContainerProfile(family) hr=0x" + std::to_string(static_cast<unsigned long>(profileHr)) + " (0=created, 0x800700B1=already exists)");
	}

	auto spawnInContainer = [&](PSID sid, const wchar_t* mode) -> DWORD
	{
		SECURITY_CAPABILITIES capabilities = {};
		capabilities.AppContainerSid = sid;

		STARTUPINFOEXW startupInfo = {};
		startupInfo.StartupInfo.cb = sizeof(startupInfo);
		SIZE_T size = 0;
		InitializeProcThreadAttributeList(nullptr, 1, 0, &size);
		std::vector<BYTE> buffer(size);
		startupInfo.lpAttributeList = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(buffer.data());
		InitializeProcThreadAttributeList(startupInfo.lpAttributeList, 1, 0, &size);
		UpdateProcThreadAttribute(startupInfo.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_SECURITY_CAPABILITIES, &capabilities, sizeof(capabilities), nullptr, nullptr);

		std::wstring commandLine = L"\"" + exe + L"\" " + mode;
		PROCESS_INFORMATION processInfo = {};
		if (!CreateProcessW(nullptr, &commandLine[0], nullptr, nullptr, FALSE, EXTENDED_STARTUPINFO_PRESENT | CREATE_NO_WINDOW, nullptr, getLocalCacheDir().c_str(), &startupInfo.StartupInfo, &processInfo))
		{
			log(std::string("Spawn FAILED for mode '") + narrow(mode) + "', err=" + std::to_string(GetLastError()));
			return static_cast<DWORD>(-1);
		}
		WaitForSingleObject(processInfo.hProcess, INFINITE);
		DWORD engineExitCode = 0;
		GetExitCodeProcess(processInfo.hProcess, &engineExitCode);
		CloseHandle(processInfo.hProcess);
		CloseHandle(processInfo.hThread);
		log(std::string("Engine exit code for mode '") + narrow(mode) + "': " + std::to_string(engineExitCode) + "  (bit1=not-AC bit2=read-fail bit4=create-fail bit8=encrypted-read-fail)");
		return engineExitCode;
	};

	const DWORD customResult = spawnInContainer(customSid, L"custom");
	const DWORD packageResult = spawnInContainer(appContainerSid, L"package");
	log(std::string("A/B VERDICT: custom-container (today's JASP) = ") + (customResult == 0 ? "all OK" : "FAILED (0x" + std::to_string(customResult) + ")") +
		", package-SID (Phase 3) = " + (packageResult == 0 ? "all OK" : "FAILED (" + std::to_string(packageResult) + ")"));

	log("================ EFSPoC launcher done =================");
	MessageBoxW(nullptr, L"Done - A/B run complete. Check poc-engine.log for the MODE=custom vs MODE=package results.", L"EFSPoC", MB_ICONINFORMATION);
	return 0;
}
