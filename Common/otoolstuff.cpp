#include "otoolstuff.h"
#include "stringutils.h"
#include <fstream>
#include <boost/algorithm/string/predicate.hpp>
#include "utils.h"
#include "appinfo.h"
#include <iostream>
#include <filesystem>

std::string _system(std::string cmd)
{
	auto path = std::filesystem::temp_directory_path() / "jaspTmpSystem";
	std::ofstream ofs(path , std::ofstream::out);
	if (!ofs.is_open())
		throw std::runtime_error("Cannot open output file for separate R/System cmd!");
	cmd += " > " + path.generic_string() + " 2>&1 ";

#ifdef WIN32
	cmd = '"' + cmd + '"'; // See: https://stackoverflow.com/questions/2642551/windows-c-system-call-with-spaces-in-command
#endif

	system(cmd.c_str());
	ofs.close();
	std::ifstream readLog(path);
	std::stringstream out;

	if(readLog)
	{
		out << readLog.rdbuf();
		readLog.close();
	}

	return out.str();
}

void _moduleLibraryFixer(const std::string & moduleLibraryPath, bool engineCall, bool printStuff, bool devMod)
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
	std::string framework_resources = "@executable_path/../Frameworks/R.framework/Versions/" + AppInfo::getRDirName() + "/Resources/";

	try
	{
		// Follow symlinks so that we may fix pkgs installed by renv 
		// (where the actual files are in cache and only symlinked in library)
		for(recIt dir(modLibpath, std::filesystem::directory_options::follow_directory_symlink); dir != recIt(); dir++)
		{
			path = dir->path();

			/*if(printStuff)
				std::cout << "- Now checking whether the path at '" << path.string() << "' is one we should fix.\n" << std::flush;*/
	
			// We only want files that have dylib or so as extension and don't have dSYM 
			// anywhere in the path (because those are some kind of debugsymbols)
			if(	! (	std::filesystem::is_regular_file(path)						&&
					(path.extension() == ".dylib" || path.extension() == ".so")	&&
					path.string().find("dSYM") == std::string::npos				))
				continue;
	
			if(printStuff)
				std::cout << "- Now checking and fixing otool paths for file '" << path.string() << "'.\n" << std::flush;
	
			std::string libPath		= stringUtils::replaceBy(path.string(), " ", "\\ "),
						otoolCmd	= "otool -L " + libPath,
						otoolOut	= _system(otoolCmd);
			auto		otoolLines	= stringUtils::split(otoolOut, '\n');

			std::string libName 	= path.stem().string() + path.extension().string();

			for(size_t i = 1; i < otoolLines.size(); i++)
			{
				std::string line = otoolLines[i];
				line = line.substr(0, line.find_first_of('('));
				stringUtils::trim(line);
				line = stringUtils::replaceBy(line, " ", "\\ ");

				if(printStuff)
					std::cout << "OTOOL: " << line << std::endl;

				// Know prefixes to be replaced
				const std::map<std::string, std::string> prefixes_map = {
					{"/Library/Frameworks/R.framework/Versions/" + AppInfo::getRDirName() + "/Resources/lib",	framework_resources + "lib"},
                    // {"/opt/R/arm64/lib",											framework_resources + "opt/R/arm64/lib"},
                    {"/usr/local/lib/libjags",										framework_resources + "opt/jags/lib/libjags"},
                    {"/usr/local/lib/libjrmath",									framework_resources + "opt/jags/lib/libjrmath"},
                    {"/usr/local/lib", 												framework_resources + "opt/local/lib"},
                    {"/opt/gfortran/lib/gcc/x86_64-apple-darwin20.0/12.2.0",		framework_resources + "lib"},
					{"/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0",		framework_resources + "opt/R/arm64/gfortran/lib"},
					{"/opt/X11/lib",												framework_resources + "opt/X11/lib"},
				};

				// Known fix library id's and paths
				std::map<std::string, std::string> ids_to_be_replaced;
				if(devMod) {
					ids_to_be_replaced = {
#ifndef __aarch64__
					{"libgfortran.dylib",					framework_resources + "opt/local/gfortran/lib/libgfortran.dylib"}
					,{"libquadmath.dylib",					framework_resources + "opt/local/gfortran/lib/libquadmath.dylib"}
#endif
						};
				}
				else {
					ids_to_be_replaced = {
						{"libtbbmalloc.dylib",					"@executable_path/../Modules/" + jaspModuleName + "/RcppParallel/lib/libtbbmalloc.dylib"},
						{"libtbbmalloc_proxy.dylib",			"@executable_path/../Modules/" + jaspModuleName + "/RcppParallel/lib/libtbbmalloc_proxy.dylib"},
						{"libtbb.dylib",						"@executable_path/../Modules/" + jaspModuleName + "/RcppParallel/lib/libtbb.dylib"}
#ifndef __aarch64__
						,{"libgfortran.dylib",					framework_resources + "opt/local/gfortran/lib/libgfortran.dylib"}
						,{"libquadmath.dylib",					framework_resources + "opt/local/gfortran/lib/libquadmath.dylib"}
#endif
					};
				}

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
					// Loop in the reverse order since "/usr/local/lib/libjags" must be handled before "/usr/local/lib"
					for(auto prefix = prefixes_map.rbegin(); prefix != prefixes_map.rend(); ++prefix)
					{
						if (stringUtils::startsWith(line, prefix->first))
						{
							install_name_tool_cmd(line, stringUtils::replaceBy(prefix->second + line.substr(prefix->first.size()), " ", "\\ "));
							break;
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

			if(!devMod)
			{
				std::cout << "Signing the modified library\n";
				const std::string sign_command = "codesign --force --deep --verbose=4 --timestamp --sign \"" + AppInfo::getSigningIdentity() + "\" " + path.string();

				if (printStuff)
					std::cout << sign_command << std::endl;

				_system(sign_command);
			}
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
