#ifndef EXTRACTARCHIVE_H
#define EXTRACTARCHIVE_H

#include <set>
#include <map>
#include <string>
#include <functional>
#include "archive.h"
#include "archive_entry.h"


/// Helper class to interface with lizip/archive to unpack archives and to get particular files from it
/// Used by DynamicModules and the ModuleInstaller for getting DESCRIPTION and/or Description.qml out of the archive when manually installing a module (from an archive)
/// This way the description can be shown before installing it, which is nice.
/// It can unpack it somewhere and make sure certain structures are removed (some nesting)
class ExtractArchive
{
public:
	static bool			extractArchiveToFolder(std::string archiveFilename, std::string destination) { return _extractArchiveToFolder(archiveFilename, destination, &stripRootDirPathModifier); }
	static bool			extractArchiveToFolderFlattened(std::string archiveFilename, std::string destination, const std::set<std::string> & exemptions);
	/*static bool			extractJaspModule(std::string archiveFilename, std::string destination, const std::map<std::string, std::set<std::string>> & folderAndExtensionExemptions);*/

	static bool			isFileAnArchive(std::string filename);
	static std::string	extractSingleTextFileFromArchive(std::string archiveFilename, std::string desiredTextFileName);

	static std::string	stripRootDirPathModifier(std::string in);

private:
	static bool			_extractArchiveToFolder(std::string archiveFilename, std::string destination, std::function<std::string(std::string)> archiveEntryPathModifier = [](std::string in){ return in;}, std::function<bool(std::string)> fileFilter = [](std::string in){ return true;});
	static bool			_extractArchiveToFolder(std::string archiveFilename, std::string destination, std::function<bool(std::string)> fileFilter) { return _extractArchiveToFolder(archiveFilename, destination, [](std::string in){ return in;}, fileFilter); }
	static int			copy_data(struct archive *ar, struct archive *aw);
	static std::string	flattenDestinationPath(const std::set<std::string> & exemptions);

						ExtractArchive()  {}
};

#endif // EXTRACTARCHIVE_H
