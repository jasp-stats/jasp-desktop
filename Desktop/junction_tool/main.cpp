#include <iostream>
#include <fstream>
#include <filesystem>
#include <string>
#include <vector>

namespace fs = std::filesystem;

#ifdef _WIN32

#include <windows.h>
#include <winioctl.h>

// --- WINDOWS KERNEL DEFINITIONS ---
typedef struct _REPARSE_DATA_BUFFER {
    ULONG  ReparseTag;
    USHORT ReparseDataLength;
    USHORT Reserved;
    union {
        struct {
            USHORT SubstituteNameOffset;
            USHORT SubstituteNameLength;
            USHORT PrintNameOffset;
            USHORT PrintNameLength;
            WCHAR  PathBuffer[1];
        } MountPointReparseBuffer;
    } DUMMYUNIONNAME;
} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;

// --- RAII WRAPPER ---
struct ScopedHandle {
    HANDLE handle;
    ScopedHandle(HANDLE h) : handle(h) {}
    ~ScopedHandle() { if (IsValid()) CloseHandle(handle); }
    bool IsValid() const { return handle != INVALID_HANDLE_VALUE; }
    operator HANDLE() const { return handle; }
};

// --- CORE LOGIC ---
std::wstring GetJunctionTarget(const fs::path& path) {
    ScopedHandle hFile(CreateFileW(path.c_str(), 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                                   NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL));
    
    if (!hFile.IsValid()) return L"";

    BYTE buffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    DWORD bytesReturned;
    
    if (DeviceIoControl(hFile, FSCTL_GET_REPARSE_POINT, NULL, 0, buffer, sizeof(buffer), &bytesReturned, NULL)) {
        auto* repoData = reinterpret_cast<PREPARSE_DATA_BUFFER>(buffer);
        if (repoData->ReparseTag == IO_REPARSE_TAG_MOUNT_POINT) {
            std::wstring target(
                repoData->MountPointReparseBuffer.PathBuffer + (repoData->MountPointReparseBuffer.SubstituteNameOffset / sizeof(WCHAR)),
                repoData->MountPointReparseBuffer.SubstituteNameLength / sizeof(WCHAR)
            );
            
            if (target.compare(0, 4, L"\\??\\") == 0) target.erase(0, 4);
            return target;
        }
    }
    return L"";
}

bool CreateTrueJunction(const fs::path& junctionLink, const fs::path& targetDir) {
    if (!fs::exists(targetDir)) {
        fs::create_directories(targetDir);
    }

    fs::path parentDir = junctionLink.parent_path();
    if (!parentDir.empty() && !fs::exists(parentDir)) {
        fs::create_directories(parentDir);
    }

    if (!fs::exists(junctionLink)) {
        fs::create_directory(junctionLink);
    }

    // Open a handle to that existing empty directory
    ScopedHandle hFile(CreateFileW(junctionLink.c_str(), GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 
                                   FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL));
    if (!hFile.IsValid()) return false;

    // Build the Reparse Data
    fs::path absoluteTarget = fs::absolute(targetDir);
    absoluteTarget.make_preferred(); 

    std::wstring ntTarget = L"\\??\\" + absoluteTarget.wstring();
    size_t ntTargetBytes = ntTarget.length() * sizeof(WCHAR);

    std::wstring printName = absoluteTarget.wstring();
    size_t printNameBytes = printName.length() * sizeof(WCHAR);

    BYTE buffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE] = {0};
    auto* reparseData = reinterpret_cast<PREPARSE_DATA_BUFFER>(buffer);

    reparseData->ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;
    reparseData->MountPointReparseBuffer.SubstituteNameOffset = 0;
    reparseData->MountPointReparseBuffer.SubstituteNameLength = static_cast<USHORT>(ntTargetBytes);
    reparseData->MountPointReparseBuffer.PrintNameOffset = static_cast<USHORT>(ntTargetBytes + sizeof(WCHAR));
    reparseData->MountPointReparseBuffer.PrintNameLength = static_cast<USHORT>(printNameBytes);
    
    memcpy(reinterpret_cast<BYTE*>(reparseData->MountPointReparseBuffer.PathBuffer) + reparseData->MountPointReparseBuffer.SubstituteNameOffset, 
           ntTarget.c_str(), ntTargetBytes + sizeof(WCHAR));
           
    memcpy(reinterpret_cast<BYTE*>(reparseData->MountPointReparseBuffer.PathBuffer) + reparseData->MountPointReparseBuffer.PrintNameOffset, 
           printName.c_str(), printNameBytes + sizeof(WCHAR));

    reparseData->ReparseDataLength = static_cast<USHORT>(8 + ntTargetBytes + sizeof(WCHAR) + printNameBytes + sizeof(WCHAR));

    // Apply the reparse point (Transforms the folder into a Junction)
    DWORD bytesReturned;
    return DeviceIoControl(hFile, FSCTL_SET_REPARSE_POINT, reparseData, 
                           reparseData->ReparseDataLength + 8, NULL, 0, &bytesReturned, NULL);
}

// --- APP MODES ---
void RunScan(const fs::path& baseDir, bool deleteAfterScan) {
    std::ofstream outFile("junctions_map.txt");
    if (!outFile) {
        std::cerr << "[ERROR] Could not create output file.\n";
        return;
    }

    auto options = fs::directory_options::skip_permission_denied;
    int count = 0;
    
    // Store paths to delete later so we don't break the iterator
    std::vector<fs::path> junctionsToDelete;

    for (const auto& entry : fs::recursive_directory_iterator(baseDir, options)) {
        DWORD attributes = GetFileAttributesW(entry.path().c_str());
        if (fs::is_directory(entry.status()) && (attributes != INVALID_FILE_ATTRIBUTES) && (attributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
            
            std::wstring targetW = GetJunctionTarget(entry.path());
            if (!targetW.empty()) {
                fs::path relFrom = entry.path().lexically_relative(baseDir);
                fs::path relTo   = fs::path(targetW).lexically_relative(baseDir); 
                
                outFile << relFrom.string() << " / " << relTo.string() << "\n";
                std::cout << "[SCAN] " << relFrom.string() << " -> " << relTo.string() << "\n";
                
                if (deleteAfterScan) {
                    junctionsToDelete.push_back(entry.path());
                }
                
                count++;
            }
        }
    }
    std::cout << "\nScan complete. Found " << count << " junctions.\n";

    // Perform the deletions if the flag was provided
    if (deleteAfterScan && !junctionsToDelete.empty()) {
        std::cout << "Starting cleanup...\n";
        int delCount = 0;
        for (const auto& juncPath : junctionsToDelete) {
            std::error_code ec;
            // fs::remove securely unlinks the junction without touching the target contents
            if (fs::remove(juncPath, ec)) {
                std::cout << "[DELETED] " << juncPath.lexically_relative(baseDir).string() << "\n";
                delCount++;
            } else {
                std::cerr << "[ERROR DELETING] " << juncPath.string() << " (" << ec.message() << ")\n";
            }
        }
        std::cout << "\nCleanup complete. Deleted " << delCount << " junctions.\n";
    }
}

void RunCreate(const std::string& filename, const fs::path& baseDirJunction, const fs::path& baseDirTarget) {
    std::ifstream inFile(filename);
    if (!inFile) { 
        std::cerr << "[ERROR] Could not open " << filename << "\n"; 
        return; 
    }

    std::string line;
    int successCount = 0, failCount = 0;

    while (std::getline(inFile, line)) {
        size_t sep = line.find(" / ");
        if (sep == std::string::npos) continue;

        fs::path relFrom = line.substr(0, sep);
        fs::path relTo   = line.substr(sep + 3);

        // Calculate independent absolute paths for junction location and target location
        fs::path absFrom = baseDirJunction / relFrom;
        fs::path absTo   = baseDirTarget / relTo;

        if (CreateTrueJunction(absFrom, absTo)) {
            std::cout << "[CREATE] " << absFrom.string() << "\n";
            successCount++;
        } else {
            std::cerr << "[ERROR] Failed on: " << absFrom.string() << " (Win32 Code: " << GetLastError() << ")\n";
            failCount++;
        }
    }

    //Handle special dirs 'manifests' and 'Tools'
    std::cout << "\nProcessing special folders and files...\n";
    std::vector<std::string> specialDirs = {"manifests", "Tools"};
    for (const auto& dirName : specialDirs) {
        fs::path targetDir = baseDirTarget / dirName;
        fs::path junctionLink = baseDirJunction / dirName;

        if (fs::exists(targetDir) && fs::is_directory(targetDir)) {
            if (CreateTrueJunction(junctionLink, targetDir)) {
                std::cout << "[CREATE SPECIAL] " << junctionLink.string() << "\n";
                successCount++;
            } else {
                std::cerr << "[ERROR] Failed on special dir: " << junctionLink.string() << " (Win32 Code: " << GetLastError() << ")\n";
                failCount++;
            }
        }
    }

    // Check for 'modules-settings.json'
    fs::path settingsFileTarget = baseDirTarget / "modules-settings.json";
    fs::path settingsFileDest = baseDirJunction / "modules-settings.json";
    if (fs::exists(settingsFileTarget) && fs::is_regular_file(settingsFileTarget)) {
        try {
            fs::copy_file(settingsFileTarget, settingsFileDest, fs::copy_options::overwrite_existing);
            std::cout << "[COPIED SPECIAL] " << settingsFileDest.string() << "\n";
        } catch (const fs::filesystem_error& e) {
            std::cerr << "[ERROR] Failed to copy modules-settings.json: " << e.what() << "\n";
            failCount++;
        }
    }
    
    std::cout << "\nCreation complete. Success: " << successCount << ", Failed: " << failCount << "\n";
}

#endif // _WIN32


// --- ENTRY POINT ---
int main(int argc, char* argv[]) {
#ifndef _WIN32
    std::cerr << "Error: This tool relies on Windows-specific NTFS features and cannot run on this OS.\n";
    return 1;
#else
    if (argc < 3) {
        std::cout << "--- NTFS Junction Tool ---\n"
                  << "Scan Mode:          " << argv[0] << " -s  <search_and_base_dir>\n"
                  << "Scan & Delete Mode: " << argv[0] << " -sd <search_and_base_dir>\n"
                  << "Create Mode:        " << argv[0] << " -c  <map_file> <base_dir_junction> <base_dir_target>\n"
                  << "\nExample:\n"
                  << "  " << argv[0] << " -sd \"C:\\JASP\\Modules\"\n"
                  << "  " << argv[0] << " -c  \"junctions_map.txt\" \"C:\\Dest\\Modules\" \"C:\\JASP\\Modules\"\n";
        return 1;
    }

    std::string mode = argv[1];
    
    try {
        if (mode == "-s") {
            RunScan(fs::absolute(argv[2]).lexically_normal(), false);
        } else if (mode == "-sd") {
            RunScan(fs::absolute(argv[2]).lexically_normal(), true);
        } else if (mode == "-c") {
            // Require 5 arguments (exe, -c, file, baseDirJunction, baseDirTarget)
            if (argc < 5) {
                std::cerr << "[ERROR] Create Mode requires a map file, a base junction directory, and a base target directory.\n";
                return 1;
            }
            RunCreate(argv[2], fs::absolute(argv[3]).lexically_normal(), fs::absolute(argv[4]).lexically_normal());
        } else {
            std::cerr << "[ERROR] Unknown mode. Use -s, -sd, or -c.\n";
            return 1;
        }
    } catch (const std::exception& e) {
        std::cerr << "[FATAL] " << e.what() << "\n";
        return 1;
    }

    return 0;
#endif
}