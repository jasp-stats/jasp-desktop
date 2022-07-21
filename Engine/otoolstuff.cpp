#include "otoolstuff.h"
#include "stringutils.h"
#include "rbridge.h"
#include <fstream>
#include <boost/algorithm/string/predicate.hpp>
#include "utils.h"
#include "appinfo.h"
#include <iostream>

std::string _system(std::string cmd)
{
	const char *root, *relativePath;

	if (!rbridge_requestTempFileName("log", &root, &relativePath))
		throw std::runtime_error("Cannot open output file for separate R/System cmd!");

	std::string path = std::string(root) + "/" + relativePath;

	cmd += " > " + path + " 2>&1 ";

#ifdef WIN32
	cmd = '"' + cmd + '"'; // See: https://stackoverflow.com/questions/2642551/windows-c-system-call-with-spaces-in-command
#endif

	system(cmd.c_str());

	std::ifstream readLog(path);
	std::stringstream out;

	if(readLog)
	{
		out << readLog.rdbuf();
		readLog.close();
	}

	return out.str();
}

void _moduleLibraryFixer(const std::string & moduleLibraryPath, bool engineCall, bool printStuff)
{
	using namespace boost;

#ifdef JASP_DEBUG
	printStuff = true; // If debugging please always print stuff
#endif

	std::filesystem::path	modLibpath	= Utils::osPath(moduleLibraryPath);

	std::cout << "modLibpath: " << modLibpath << std::endl;

	std::string jaspModuleBinaryPath;
	std::string jaspModuleName;
	if (modLibpath.string().find("/Modules/jasp") != std::string::npos) 
	{
		auto modulePlace = modLibpath.string().find("/Modules/") + 9;
		auto moduleNameLength = modLibpath.string().find('/', modulePlace);

		jaspModuleName = modLibpath.string().substr(modulePlace, moduleNameLength - modulePlace);
		jaspModuleBinaryPath = modLibpath.string().substr(0, modulePlace);
	}

#ifdef __APPLE__
	std::cout << "This is a mac so we will fix the otool mess of folder '" << modLibpath << "'...\n";

	typedef std::filesystem::recursive_directory_iterator	recIt;
	
	std::filesystem::path path;
	
	try
	{
		// Follow symlinks so that we may fix pkgs installed by renv 
		// (where the actual files are in cacche and only symlinked in library)
		for(recIt dir(modLibpath, std::filesystem::directory_options::follow_directory_symlink); dir != recIt(); dir++)
		{
			path = dir->path();
	
			// We only want files that have dylib or so as extension and don't have dSYM 
			// anywhere in the path (because those are some kind of debugsymbols)
			if(	! (	std::filesystem::is_regular_file(path)							&&
					(path.extension() == ".dylib" || path.extension() == ".so")	&&
					path.string().find("dSYM") == std::string::npos				))
				continue;
	
			if(printStuff)
				std::cout << "- Now checking and fixing otool paths for file '" << path.string() << "'.\n";
	
			std::string libPath		= stringUtils::replaceBy(path.string(), " ", "\\ "),
						otoolCmd	= "otool -L " + libPath,
						otoolOut	= _system(otoolCmd);
			auto		otoolLines	= stringUtils::splitString(otoolOut, '\n');

			std::string libName 	= path.stem().string() + path.extension().string();
	
			for(size_t i = 1; i < otoolLines.size(); i++)
			{
				std::string line = otoolLines[i];
				line = line.substr(0, line.find_first_of('('));
				stringUtils::trim(line);
				line = stringUtils::replaceBy(line, " ", "\\ ");

				// Know prefixes to be replaced
				// - jags paths are fixed by CMake 
				const std::map<std::string, std::string> prefixes_map = {
					{"/Library/Frameworks/R.framework/Versions/" + AppInfo::getRDirName() + "/Resources/lib", "@executable_path/../Frameworks/R.framework/Versions/" + AppInfo::getRDirName() + "/Resources/lib"}
					// {"/opt/R/arm64/lib", "@executable_path/../Frameworks/R.framework/Versions/" + AppInfo::getRDirName() + "/Resources/opt/R/arm64/lib"},
					// {"/usr/local/lib", "@executable_path/../Frameworks/R.framework/Versions/" + AppInfo::getRDirName() + "/Resources/opt/local/lib"},
				};

				// Known fix library id's and paths 
				const std::map<std::string, std::string> ids_to_be_replaced =
				{
					{"libtbbmalloc.dylib",					"@executable_path/../Modules/" + jaspModuleName + "/RcppParallel/lib/libtbbmalloc.dylib"},
					{"libtbbmalloc_proxy.dylib",			"@executable_path/../Modules/" + jaspModuleName + "/RcppParallel/lib/libtbbmalloc_proxy.dylib"},
					{"libtbb.dylib",						"@executable_path/../Modules/" + jaspModuleName + "/RcppParallel/lib/libtbb.dylib"}
				};

				auto install_name_tool_cmd = [&](const std::string & replaceThisLine, const std::string & withThisLine)
				{
					const std::string cmd = "install_name_tool -change " + replaceThisLine + " " + withThisLine + " " + libPath;
	
					if(printStuff)
						std::cout << cmd << std::endl;
	
					_system(cmd);
				};

				auto install_name_tool_id_cmd = [&](const std::string & newId)
				{
					const std::string cmd = "install_name_tool -id " + newId + " " + libPath;
	
					if(printStuff)
						std::cout << cmd << std::endl;
	
					_system(cmd);
				};

				auto install_name_tool_delete_rpath_cmd = [&](const std::string & rpath)
				{
					const std::string cmd = "install_name_tool -delete_rpath " + rpath + " " + libPath;
	
					if(printStuff)
						std::cout << cmd << std::endl;
	
					_system(cmd);
				};

				if (!stringUtils::startsWith(line, "@executable_path/../"))
				{

					// Replacing the known prefixes
					for(auto &prefix : prefixes_map) 
					{
						if (stringUtils::startsWith(line, prefix.first)) 
						{
							install_name_tool_cmd(line, stringUtils::replaceBy(prefix.second + line.substr(prefix.first.size()), " ", "\\ "));
						}
					}

					// Replacing the known fixed paths, and id's
					for(auto &entry : ids_to_be_replaced) 
					{
						if (boost::algorithm::ends_with(line, entry.first))
						{
							if (boost::algorithm::ends_with(line, libName)) {
								install_name_tool_id_cmd(entry.second);
							}
							install_name_tool_cmd(line, entry.second);
						}
					}
				}
				
			}

			std::cout << "Signing the modified library\n";
			const std::string sign_command = "codesign --force --deep --verbose=4 --timestamp --sign \"" + AppInfo::getSigningIdentity() + "\" " + path.string();

			if (printStuff)
				std::cout << sign_command << std::endl;

			_system(sign_command);
		}
	}
	catch(std::filesystem::filesystem_error & error)
	{
		std::cout << "Filesystem iterating had error: '" << error.what() << "' last path was: '" << path.string() << "'" << std::endl;
	}

#else
	std::cout << "This isn't a mac so we aren't trying to fix the otool mess..." << std::endl;
#endif
}
