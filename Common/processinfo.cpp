//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "processinfo.h"

#ifdef _WIN32
#include <windows.h>
#include <tlhelp32.h>
#else
#include "unistd.h"
#endif

unsigned long ProcessInfo::currentPID()
{

#ifdef _WIN32
	return GetCurrentProcessId();
#else
	return getpid();
#endif
}

unsigned long ProcessInfo::parentPID()
{

#ifdef _WIN32

	HANDLE h = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
	PROCESSENTRY32 pe = { 0 };
	pe.dwSize = sizeof(PROCESSENTRY32);

    unsigned long pid = currentPID();
	unsigned long ppid = 0;

	if( Process32First(h, &pe)) {
		do {
			if (pe.th32ProcessID == pid) {
				ppid = pe.th32ParentProcessID;
				break;
			}
		} while( Process32Next(h, &pe));
	}

	CloseHandle(h);

	return ppid;

#else

	return getppid();

#endif
}

bool ProcessInfo::isParentRunning()
{
#ifdef _WIN32

    static unsigned long _parentPID = parentPID();
	static void* _parentHandle = NULL;

	if (_parentHandle == NULL && _parentPID != 0)
		_parentHandle = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, _parentPID);

	if (_parentHandle != NULL)
	{
		BOOL success;
		DWORD exitCode;

		success = GetExitCodeProcess(_parentHandle, &exitCode);

		return ( ! success) || exitCode == STILL_ACTIVE;
	}
#else
	return getppid() != 1;
#endif
}
