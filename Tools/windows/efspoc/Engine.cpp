//EFSPoC engine: spawned by the launcher inside an AppContainer running under JASP.EFSPoC's own
//package-family SID (standing in for JASPEngine under Phase 3). It answers the spike questions from
//inside the sandbox and logs to LocalCache\poc-engine.log:
//  - is it really an AppContainer, and under which SID?
//  - does its token still carry the WIN://SYSAPPID package-identity claims?
//  - THE MONEY TEST: can it read a file the full-trust launcher created in the (possibly
//    "Application Protected" EFS-encrypted) LocalCache, and can it create a new file there?
//Exit code: 0 = everything OK; bit 1 = not an AppContainer; bit 2 = read failed; bit 4 = create failed.

#define _WIN32_WINNT 0x0A00
#include <windows.h>
#include <appmodel.h>
#include <sddl.h>
#include <string>
#include <cstring>
#include <vector>

#pragma comment(lib, "advapi32.lib")

//GetTokenInformation(TokenSecurityAttributes) structures are documented under ntifs.h (WDK) and absent from
//the public user-mode SDK, so they are re-declared here exactly as documented (stable since Windows 8).
namespace
{
	enum { kTokenTypeString = 0x03 };

	typedef union _POC_TOKEN_SECURITY_ATTRIBUTE_VALUE {
		LONG_PTR	plnteger;
		ULONG_PTR	pUlinteger;
		PWCHAR		pString;
	} POC_TOKEN_SECURITY_ATTRIBUTE_VALUE;

	typedef struct _POC_TOKEN_SECURITY_ATTRIBUTE_V1 {
		PWSTR							pName;
		USHORT						ValueType;
		USHORT						Reserved;
		ULONG						Flags;
		DWORD						ValueCount;
		POC_TOKEN_SECURITY_ATTRIBUTE_VALUE*	Values;
	} POC_TOKEN_SECURITY_ATTRIBUTE_V1;

	typedef struct _POC_TOKEN_SECURITY_ATTRIBUTES_INFORMATION {
		USHORT						Version;
		USHORT						Reserved;
		DWORD						AttributeCount;
		POC_TOKEN_SECURITY_ATTRIBUTE_V1*	Attributes;
	} POC_TOKEN_SECURITY_ATTRIBUTES_INFORMATION;
}

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

	std::wstring getFamilyName()
	{
		WCHAR familyName[PACKAGE_FAMILY_NAME_MAX_LENGTH + 1] = {};
		UINT32 length = ARRAYSIZE(familyName);
		if (GetCurrentPackageFamilyName(&length, familyName) != ERROR_SUCCESS)
			return L"";
		return familyName;
	}
}

static int runProbe();

static void logSeh(unsigned int exceptionCode);
static void logHeartbeat();
static std::wstring g_mode = L"unspecified";

int main(int argc, wchar_t* argv[])
{
	//The launcher tells us which identity it spawned us under: "custom" (= JASP today) or "package" (= Phase 3).
	if (argc > 1 && argv[1])
		g_mode = argv[1];
	//First-instruction heartbeat: cwd is the LocalCache dir (the launcher sets it), so a relative log proves
	//we got here even if everything after crashes. (No object construction in this function: C2712.)
	g_logPath = L"poc-engine-heartbeat.log";
	logHeartbeat();

	int exitCode = 0;
	__try
	{
		exitCode = runProbe();
	}
	__except (logSeh(GetExceptionCode()), EXCEPTION_EXECUTE_HANDLER)
	{
		exitCode = 99;
	}
	return exitCode;
}

static void logHeartbeat()
{
	log("engine alive, entry reached");
}

static void logSeh(unsigned int exceptionCode)
{
	//Write directly to the relative heartbeat file: g_logPath may be unusable at this point.
	HANDLE file = CreateFileW(L"poc-engine-heartbeat.log", FILE_APPEND_DATA, FILE_SHARE_READ, nullptr, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
	if (file == INVALID_HANDLE_VALUE) return;
	char buffer[64] = {};
	DWORD written = 0;
	_snprintf(buffer, sizeof(buffer) - 1, "SEH EXCEPTION 0x%08X", exceptionCode);
	WriteFile(file, buffer, static_cast<DWORD>(strlen(buffer)), &written, nullptr);
	WriteFile(file, "\r\n", 2, &written, nullptr);
	CloseHandle(file);
}

//Crumb trail appended to the relative heartbeat file (proven writable from inside the container) so we can
//see exactly how far runProbe gets even when the absolute-path log cannot be written or the process dies.
static void crumb(const char* stage)
{
	HANDLE file = CreateFileW(L"poc-engine-heartbeat.log", FILE_APPEND_DATA, FILE_SHARE_READ, nullptr, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
	if (file == INVALID_HANDLE_VALUE) return;
	DWORD written = 0;
	WriteFile(file, stage, static_cast<DWORD>(strlen(stage)), &written, nullptr);
	WriteFile(file, "\r\n", 2, &written, nullptr);
	CloseHandle(file);
}

static int runProbe()
{
	crumb("probe: computing localCache");
	const std::wstring familyName = getFamilyName();
	crumb(familyName.empty() ? "probe: family name EMPTY - AC child has NO package identity (GetCurrentPackageFamilyName failed)" : "probe: family name available in AC child");
	const std::wstring localCache = []() {
		WCHAR localAppData[MAX_PATH] = {};
		GetEnvironmentVariableW(L"LOCALAPPDATA", localAppData, MAX_PATH);
		return std::wstring(localAppData) + L"\\Packages\\" + getFamilyName() + L"\\LocalCache";
	}();
	crumb("probe: localCache computed");

	//The launcher sets our working directory to the LocalCache, so relative logging works no matter what
	//environment or package-identity quirks the container child exhibits.
	g_logPath = L"poc-engine.log";
	const std::wstring launcherFile = L"launcher-file.txt";
	const std::wstring engineFile = L"engine-created.txt";
	log(std::string("---------------- EFSPoC engine start, MODE=") + narrow(g_mode) + " ----------------");
	crumb("probe: main log written");

	int exitCode = 0;

	//MONEY TESTS FIRST - they are the actual point; forensics must not be able to take them down.
	crumb("probe: money test 1 (read)");
	HANDLE file = CreateFileW(launcherFile.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);
	if (file == INVALID_HANDLE_VALUE)
	{
		log("MONEY TEST read launcher-file.txt: FAILED err=" + std::to_string(GetLastError()) + " (6002=ERROR_FILE_ENCRYPTED 5=access denied)");
		exitCode |= 2;
	}
	else
	{
		char buffer[32] = {};
		DWORD bytesRead = 0;
		ReadFile(file, buffer, sizeof(buffer) - 1, &bytesRead, nullptr);
		CloseHandle(file);
		log(std::string("MONEY TEST read launcher-file.txt: OK, content='") + buffer + "'");
	}
	crumb("probe: money test 1 done");

	//MONEY TEST 2: create a new file in LocalCache (cwd) from inside the container.
	crumb("probe: money test 2 (create)");
	file = CreateFileW(engineFile.c_str(), GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
	if (file == INVALID_HANDLE_VALUE)
	{
		log("MONEY TEST create engine-created.txt: FAILED err=" + std::to_string(GetLastError()) + " (6000=ERROR_ENCRYPTION_FAILED encrypt-at-create refused)");
		exitCode |= 4;
	}
	else
	{
		const char data[] = "engine-was-here";
		DWORD written = 0;
		WriteFile(file, data, sizeof(data), &written, nullptr);
		CloseHandle(file);
		log("MONEY TEST create engine-created.txt: OK");
	}
	crumb("probe: money test 2 done");

	//MONEY TEST 3: read a file from the (Application Protected EFS-encrypted) install tree. WindowsApps grants
	//all AppContainers read access via AAP ACEs, so a failure here is EFS refusing the identity, not the ACL.
	crumb("probe: money test 3 (read encrypted install file)");
	{
		WCHAR modulePath[MAX_PATH] = {};
		GetModuleFileNameW(nullptr, modulePath, MAX_PATH);
		std::wstring manifest(modulePath);
		manifest.replace(manifest.find_last_of(L'\\') + 1, std::wstring::npos, L"AppxManifest.xml");
		HANDLE manifestFile = CreateFileW(manifest.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);
		if (manifestFile == INVALID_HANDLE_VALUE)
		{
			log("MONEY TEST read encrypted AppxManifest.xml: FAILED err=" + std::to_string(GetLastError()) + " (6002=ERROR_FILE_ENCRYPTED 5=access denied)");
			exitCode |= 8;
		}
		else
		{
			char buffer[64] = {};
			DWORD bytesRead = 0;
			ReadFile(manifestFile, buffer, sizeof(buffer) - 1, &bytesRead, nullptr);
			CloseHandle(manifestFile);
			log(std::string("MONEY TEST read encrypted AppxManifest.xml: OK, first bytes='") + buffer + "'");
		}
	}
	crumb("probe: money test 3 done");

	//Token forensics: AppContainer? which SID?
	crumb("probe: opening process token");
	HANDLE token = nullptr;
	if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token))
	{
		DWORD isAppContainer = 0, size = 0;
		GetTokenInformation(token, TokenIsAppContainer, &isAppContainer, sizeof(isAppContainer), &size);
		log(std::string("TokenIsAppContainer: ") + (isAppContainer ? "yes" : "NO"));
		if (!isAppContainer) exitCode |= 1;

		if (isAppContainer)
		{
			PTOKEN_APPCONTAINER_INFORMATION containerInfo = nullptr;
			GetTokenInformation(token, TokenAppContainerSid, nullptr, 0, &size);
			containerInfo = reinterpret_cast<PTOKEN_APPCONTAINER_INFORMATION>(LocalAlloc(LPTR, size));
			if (containerInfo && GetTokenInformation(token, TokenAppContainerSid, containerInfo, size, &size) && containerInfo->TokenAppContainer)
			{
				LPWSTR stringSid = nullptr;
				if (ConvertSidToStringSidW(containerInfo->TokenAppContainer, &stringSid))
				{
					log("AppContainer SID: " + narrow(stringSid));
					LocalFree(stringSid);
				}
			}
			if (containerInfo) LocalFree(containerInfo);
			crumb("probe: appcontainer-sid dumped");
		}

		//Package identity claims (WIN://SYSAPPID): struct-based parsing crashed on Win11 25H2, so dump the raw
		//bytes and parse offline instead - the buffer layout there does not match the documented V1 shapes.
		GetTokenInformation(token, TokenSecurityAttributes, nullptr, 0, &size);
		log("TokenSecurityAttributes buffer size: " + std::to_string(size));
		BYTE* raw = reinterpret_cast<BYTE*>(LocalAlloc(LPTR, size));
		if (raw && GetTokenInformation(token, TokenSecurityAttributes, raw, size, &size))
		{
			const DWORD dumpSize = size < 2048 ? size : 2048;
			std::string hex;
			hex.reserve(dumpSize * 3);
			char byte[4] = {};
			for (DWORD i = 0; i < dumpSize; i++)
			{
				_snprintf(byte, sizeof(byte), "%02X ", raw[i]);
				hex += byte;
			}
			log("TokenSecurityAttributes raw hex: " + hex);
		}
		if (raw) LocalFree(raw);
		CloseHandle(token);
		crumb("probe: security attributes hex-dumped");
	}
	else
		log("OpenProcessToken failed, err=" + std::to_string(GetLastError()));

	log(std::string("Engine exit code: ") + std::to_string(exitCode));
	log("---------------- EFSPoC engine done -----------------");
	return exitCode;
}
