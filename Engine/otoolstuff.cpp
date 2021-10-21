#include "otoolstuff.h"
#include "stringutils.h"
#include "rbridge.h"
#include <fstream>
#include <boost/filesystem.hpp>
#include "boost/nowide/fstream.hpp"
#include "utils.h"
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

	boost::nowide::ifstream readLog(path);
	std::stringstream out;

	if(readLog)
	{
		out << readLog.rdbuf();
		readLog.close();
	}

	return out.str();
}

// TODO: This should be handled from the CMake. 
// But, I am thinking to replace these MACROs and maybe find something better
#define CURRENT_R_VERSION "4.1"
#define MAC_LIB_FOLDER "Frameworks/R.framework/Versions/"  CURRENT_R_VERSION "/Resources/lib"
#define MAC_LIB_FOLDER_FROM_EXE "@executable_path/../" MAC_LIB_FOLDER "/"
#define MAC_LIB_FOLDER_FROM_MODULE "../../../" MAC_LIB_FOLDER "/"

void _moduleLibraryFixer(const std::string & moduleLibraryPath, bool engineCall, bool printStuff)
{
	using namespace boost;
	
#ifdef JASP_DEBUG
	printStuff = true; //If debugging please always print stuff
#endif

	filesystem::path	modLibpath	= Utils::osPath(moduleLibraryPath),
						rcppPath	= Utils::osPath(moduleLibraryPath + "/Rcpp");

	//I still think it is a bad plan to have Rcpp installed doubly, so lets remove it from the module directory if its there
	if(!engineCall && exists(rcppPath))
		remove_all(rcppPath);

#ifdef __APPLE__
	std::cout << "This is a mac so we will fix the otool mess of folder '" << modLibpath << "'...\n";

	typedef filesystem::recursive_directory_iterator	recIt;
	
	filesystem::path path;
	
	try
	{
		for(recIt dir(modLibpath, filesystem::symlink_option::recurse); dir != recIt(); dir++) //Follow symlinks so that we may fix pkgs installed by renv (where the actual files are in cacche and only symlinked in library)
		{
			path = dir->path();
	
			//We only want files that have dylib or so as extension and don't have dSYM anywhere in the path (because those are some kind of debugsymbols)
			if(	! (	filesystem::is_regular_file(path)							&&
					(path.extension() == ".dylib" || path.extension() == ".so")	&&
					path.string().find("dSYM") == std::string::npos				))
				continue;
	
			if(printStuff)
				std::cout << "- Now checking and fixing otool paths for file '" << path.string() << "'.\n";
	
			std::string libDir		= stringUtils::replaceBy(path.string(), " ", "\\ "),
						otoolCmd	= "otool -L " + libDir,
						otoolOut	= _system(otoolCmd);
			auto		otoolLines	= stringUtils::splitString(otoolOut, '\n');
	
			/*if(printStuff)
			{
				std::cout << "- jaspRCPP_postProcessLocalPackageInstall used otool -L on " << libDir;
				std::cout << " and found this output:\n";
		
				for(const auto & line : otoolLines)
					std::cout << line << std::endl;
			
			}*/
			
			//ok otoolLines[1] represents the "id" of the lib but we do not need to change it because it probably points directly back to itself. The other lines however we should change
	
			for(size_t i=2; i<otoolLines.size(); i++)
			{
				std::string line = otoolLines[i];
				line = line.substr(0, line.find_first_of('('));
				stringUtils::trim(line);
				line = stringUtils::replaceBy(line, " ", "\\ ");
	
				//For all the libs of R we have the following startsWith we can check:
				const std::string libStart = "/Library/Frameworks/R.framework/Versions/";

				//For the JAGS stuff we need to replace like so:
				const std::map<std::string, std::string> replaceThese =
				{
					{	"/usr/local/lib/libjags.4.dylib",				"@executable_path/JAGS/libjags.4.dylib"		},
					{	"/usr/local/lib/libjrmath.0.dylib",				"@executable_path/JAGS/libjrmath.0.dylib"		},
					{	"/usr/local/gfortran/lib/libgfortran.5.dylib",	MAC_LIB_FOLDER_FROM_EXE "libgfortran.5.dylib"		},
					{	"/usr/local/gfortran/lib/libquadmath.0.dylib",	MAC_LIB_FOLDER_FROM_EXE "libquadmath.0.dylib"		}
				/*	R 4 doesnt have the following anymore:
					{	"/usr/lib/libc++abi.dylib",			MAC_LIB_FOLDER_FROM_EXE "libc++abi.1.dylib"	},
					{	"/usr/lib/libc++.1.dylib",			MAC_LIB_FOLDER_FROM_EXE "libc++.1.dylib"		} */
				};
				//This ought to be sort of mirrored in jasp-required-files/MacOS/Frameworks/create-framework.py
	
				auto install_name_tool_cmd = [&](const std::string & replaceThisLine, const std::string & withThisLine)
				{
					const std::string cmd = "install_name_tool -change " + replaceThisLine + " " + withThisLine + " " + libDir;
	
					if(printStuff)
						std::cout << cmd << std::endl;
	
					_system(cmd);
				};
				
				if(stringUtils::startsWith(line, MAC_LIB_FOLDER_FROM_EXE)) //This binary was already fixed
				{
					if (printStuff)
						std::cout << "Already fixed: " << line << std::endl;
				}
				else if(replaceThese.count(line) > 0)
				{
					install_name_tool_cmd(line, replaceThese.at(line));
				}
				else if(stringUtils::startsWith(line, libStart))
				{
					install_name_tool_cmd(line, stringUtils::replaceBy("@executable_path/../Frameworks/R.framework/Versions/" + line.substr(libStart.size()), " ", "\\ "));
				}
				else if(stringUtils::startsWith(line, "/opt/") || stringUtils::startsWith(line, "/usr/local/"))
				{
					std::string baseName	= line.substr(line.find_last_of('/') == std::string::npos ? 0 : line.find_last_of('/') + 1),
								newLine		= stringUtils::replaceBy(MAC_LIB_FOLDER_FROM_EXE + baseName, " ", "\\ ");
					filesystem::path	libPath	= Utils::osPath(moduleLibraryPath + "/" + MAC_LIB_FOLDER_FROM_MODULE + baseName);

					if (exists(libPath))
						install_name_tool_cmd(line, newLine);
					else if (printStuff)
						std::cout << "Cannot find library " << baseName << " in JASP libraries: " << line << std::endl;
				}
				else if (printStuff)
					std::cout << "Line not changed: " << line << std::endl;
			}
		}
	}
	catch(boost::filesystem::filesystem_error & error)
	{
		std::cout << "Filesystem iterating had error: '" << error.what() << "' last path was: '" << path.string() << "'" << std::endl;
	}

#else
	std::cout << "This isn't a mac so we aren't trying to fix the otool mess..." << std::endl;
#endif
}
