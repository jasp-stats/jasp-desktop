//Real-I/O permission probe for the JASP engine AppContainer.
//
//The old checker only called std::filesystem::status(): a metadata existence check that never opens or creates
//anything, so it was structurally blind to EFS (failures happen at open/create time) and to plenty of real ACL
//problems. This one performs actual file I/O from inside the container it is spawned in:
//
//  -r  (read-only, used for the install dir): enumerate the directory, open the first regular file and read a
//      few bytes. Never writes anything.
//  -rw (scratch dirs: temp/appData/JASP_Sandbox/...): the read probe above, plus create -> write -> read-back
//      -> delete of a uniquely named probe file. If a directory holds no regular files at all, the write probe
//      alone still proves read+write, so empty scratch dirs do not fail.
//
//Different probes catch different failure modes: the create-probe fires when a directory is EFS-encrypted and the
//container cannot encrypt at creation ("Application Protected" / classic user EFS, see jasp-issues#4566 and #3562);
//the existing-file-read probe fires when files written by another identity cannot be decrypted.
//
//Usage:  ContainerFilePermissionChecker -r|-rw <path> [<path> ...]
//
//Exit codes:
//   0                      all probes passed
//  -1                      usage error (bad arguments)
//  -2                      -r mode: a path contained no regular file to read-probe
//  (stage<<24)|(idx<<16)|err   first failing probe: stage ids below, idx = index of the path argument, err = GetLastError()
//
//The stage/path/error packing must match the decoding table in wincontainermanager.cpp (checkIfAccessible).

#include <windows.h>
#include <cstdio>
#include <cstring>
#include <string>

namespace
{
	constexpr int kStageEnumerate		= 1;	// listing the directory failed
	constexpr int kStageOpenRead		= 2;	// opening an existing regular file failed
	constexpr int kStageRead			= 3;	// reading an existing regular file failed
	constexpr int kStageCreateProbe		= 4;	// creating the probe file failed (classic EFS-can't-encrypt signature)
	constexpr int kStageWriteProbe		= 5;	// writing the probe file failed
	constexpr int kStageReopenProbe		= 6;	// reopening the just-written probe file failed
	constexpr int kStageReadbackProbe	= 7;	// reading back the probe file failed or content mismatched
	constexpr int kStageDeleteProbe		= 8;	// deleting the probe file failed

	int fail(int stage, int pathIndex, DWORD error)
	{
		fprintf(stderr, "ContainerFilePermissionChecker: stage=%d pathIndex=%d winError=%lu\n", stage, pathIndex, static_cast<unsigned long>(error));
		return (stage << 24) | ((pathIndex & 0xFF) << 16) | static_cast<int>(error & 0xFFFF);
	}

	std::wstring normalizePath(const wchar_t* raw)
	{
		std::wstring path(raw);
		for (auto& c : path)
			if (c == L'/') c = L'\\';
		while (path.size() > 3 && (path.back() == L'\\' || path.back() == L'/'))
			path.pop_back();
		return path;
	}

	int probeRead(const std::wstring& dir, int pathIndex, bool& sawRegularFile)
	{
		WIN32_FIND_DATAW findData;
		HANDLE find = FindFirstFileW((dir + L"\\*").c_str(), &findData);
		if (find == INVALID_HANDLE_VALUE)
			return fail(kStageEnumerate, pathIndex, GetLastError());

		int result = 0;
		do
		{
			if (findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
				continue;

			sawRegularFile = true;

			HANDLE file = CreateFileW((dir + L"\\" + findData.cFileName).c_str(), GENERIC_READ,
							FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, nullptr, OPEN_EXISTING, 0, nullptr);
			if (file == INVALID_HANDLE_VALUE)
			{
				result = fail(kStageOpenRead, pathIndex, GetLastError());
				break;
			}

			char buffer[16];
			DWORD bytesRead = 0;
			if (!ReadFile(file, buffer, sizeof(buffer), &bytesRead, nullptr))
				result = fail(kStageRead, pathIndex, GetLastError());

			CloseHandle(file);
			break; //one successfully read regular file proves read access
		}
		while (FindNextFileW(find, &findData));

		FindClose(find);
		return result;
	}

	int probeWrite(const std::wstring& dir, int pathIndex)
	{
		const std::wstring probePath = dir + L"\\.jasp_probe_" + std::to_wstring(GetCurrentProcessId()) + L"_" + std::to_wstring(GetTickCount64());
		const char probeData[] = "jasp-container-permission-probe";

		HANDLE file = CreateFileW(probePath.c_str(), GENERIC_WRITE, 0, nullptr, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, nullptr);
		if (file == INVALID_HANDLE_VALUE)
			return fail(kStageCreateProbe, pathIndex, GetLastError());

		DWORD bytesWritten = 0;
		if (!WriteFile(file, probeData, sizeof(probeData), &bytesWritten, nullptr) || bytesWritten != sizeof(probeData))
		{
			const DWORD error = GetLastError();
			CloseHandle(file);
			DeleteFileW(probePath.c_str());
			return fail(kStageWriteProbe, pathIndex, error);
		}
		CloseHandle(file);

		file = CreateFileW(probePath.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);
		if (file == INVALID_HANDLE_VALUE)
		{
			const DWORD error = GetLastError();
			DeleteFileW(probePath.c_str());
			return fail(kStageReopenProbe, pathIndex, error);
		}

		char buffer[64] = {};
		DWORD bytesRead = 0;
		const BOOL readOk = ReadFile(file, buffer, sizeof(probeData), &bytesRead, nullptr);
		CloseHandle(file);

		const bool contentOk = readOk && bytesRead == sizeof(probeData) && memcmp(buffer, probeData, sizeof(probeData)) == 0;
		if (!contentOk)
		{
			DeleteFileW(probePath.c_str());
			return fail(kStageReadbackProbe, pathIndex, readOk ? ERROR_INVALID_DATA : GetLastError());
		}

		if (!DeleteFileW(probePath.c_str()))
			return fail(kStageDeleteProbe, pathIndex, GetLastError());

		return 0;
	}
}

int wmain(int argc, wchar_t* argv[])
{
	if (argc < 3 || (wcscmp(argv[1], L"-r") != 0 && wcscmp(argv[1], L"-rw") != 0))
	{
		fprintf(stderr, "Usage: ContainerFilePermissionChecker -r|-rw <path> [<path> ...]\n");
		return -1;
	}

	const bool readWrite = wcscmp(argv[1], L"-rw") == 0;

	for (int i = 2; i < argc; i++)
	{
		const std::wstring dir = normalizePath(argv[i]);
		const int pathIndex = i - 2;

		//A missing directory is not a permission problem (several granted dirs are only created on demand),
		//so skip those instead of failing them: the old fs::status() checker passed them too. Access-denied
		//errors must NOT be skipped - they are exactly the signal we are probing for.
		const DWORD attributes = GetFileAttributesW(dir.c_str());
		if (attributes == INVALID_FILE_ATTRIBUTES)
		{
			const DWORD error = GetLastError();
			if (error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND)
				continue;
		}

		bool sawRegularFile = false;
		int result = probeRead(dir, pathIndex, sawRegularFile);
		if (result != 0)
			return result;

		if (readWrite)
		{
			result = probeWrite(dir, pathIndex);
			if (result != 0)
				return result;
		}
		else if (!sawRegularFile)
			return fail(kStageOpenRead, pathIndex, ERROR_FILE_NOT_FOUND); //read-only dirs are expected to hold files
	}

	return 0;
}
