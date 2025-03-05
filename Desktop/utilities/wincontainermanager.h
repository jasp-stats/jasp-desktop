#ifdef _WIN32

#ifndef WINCONTAINER_MAN_H
#define WINCONTAINER_MAN_H

#include<filesystem>
#include<string>
#include<vector>
#include<map>


class WinContainerManager
{
protected:
    WinContainerManager(){};
	~WinContainerManager();
    static WinContainerManager* _instance;

	class Destroyer
	{
	public:
		Destroyer(WinContainerManager* s = nullptr) : _singleton(s) {};
		~Destroyer() { delete _singleton;};
		WinContainerManager *_singleton;
	};
	static Destroyer* _destroyer; // this is static so at end of program life the destructor is called on all containers


public: //NOTE lets not leak windows crap out of this class!!!!
	static WinContainerManager* getInstance();
    WinContainerManager(WinContainerManager &other) = delete;
    void operator=(const WinContainerManager &) = delete;


    enum class Permissions {
        ALL,
        NETWORK_ONLY,
        NONE
    };

	struct win32StartupArgs {
		unsigned long flags;
		const void* startupInfo;
	};

	bool containerExists(const std::string& containerName);
	bool createNewContainer(const std::string& name, Permissions permissions, const std::vector<std::filesystem::path>& accessibleDirs, const std::vector<std::filesystem::path>& accessibleFiles);
	bool getWin32StartupArgs(const std::string& containerName, win32StartupArgs& args);
	// bool launchExecInContainer(const std::string containerName, const std::filesystem::path pathToExec);
    //TODO:
    // bool addAccessibleDirs(std::string containerName, const std::vector<std::filesystem::path>& accessibleDirs);
    // bool addAccessibleFiles(std::string containerName, const std::vector<std::filesystem::path>& accessibleFiles);

private:

    class Container;
	std::map<std::string, Container*> nameToContainer;



};

#endif // WINCONTAINER_MAN_H

#endif
