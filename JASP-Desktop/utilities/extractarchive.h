#ifndef EXTRACTARCHIVE_H
#define EXTRACTARCHIVE_H

#include <set>
#include <string.h>
#include <functional>
#include "libzip/archive.h"
#include "libzip/archive_entry.h"

class ExtractArchive
{
public:
	static bool			extractArchiveToFolder(std::string archiveFilename, std::string destination) { return _extractArchiveToFolder(archiveFilename, destination); }
	static bool			extractArchiveToFolderFlattened(std::string archiveFilename, std::string destination, const std::set<std::string> & exemptions);

	static bool			isFileAnArchive(std::string filename);
	static std::string	extractSingleTextFileFromArchive(std::string archiveFilename, std::string desiredTextFileName);

private:
	static bool			_extractArchiveToFolder(std::string archiveFilename, std::string destination, std::function<std::string(std::string)> archiveEntryPathModifier = [](std::string in){ return in;});
	static int			copy_data(struct archive *ar, struct archive *aw);
	static std::string	flattenDestinationPath(const std::set<std::string> & exemptions);

						ExtractArchive()  {}
};

#endif // EXTRACTARCHIVE_H
