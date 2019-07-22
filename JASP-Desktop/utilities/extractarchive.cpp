#include "extractarchive.h"

#include <cstring>
#include <sstream>
#include "stringutils.h"
#include "log.h"

int ExtractArchive::copy_data(struct archive *ar, struct archive *aw)
{
	int r;
	const void *buff;
	size_t size;
	int64_t offset;

	while(true)
	{
		r = archive_read_data_block(ar, &buff, &size, &offset);

		if (r == ARCHIVE_EOF)
			return (ARCHIVE_OK);

		if (r < ARCHIVE_OK)
			return (r);

		r = archive_write_data_block(aw, buff, size, offset);

		if (r < ARCHIVE_OK)
		{
			Log::log() << "Problem copying data in archive: " << archive_error_string(aw) << std::endl;
			return (r);
		}
	}
}

std::string	ExtractArchive::stripRootDirPathModifier(std::string in)
{
	std::string::size_type firstDiv = in.find_first_of('/');

	if(firstDiv == std::string::npos) return in;

	return in.substr(firstDiv + 1);
}

bool ExtractArchive::_extractArchiveToFolder(std::string archiveFilename, std::string destination, std::function<std::string(std::string)> archiveEntryPathModifier, std::function<bool(std::string)> fileFilter)
{
	// https://github.com/libarchive/libarchive/wiki/Examples#A_Complete_Extractor
	if(destination.size() == 0 || destination[destination.size() - 1] != '/')
		destination += '/';

	struct archive_entry *entry;
	int flags;
	int r;

	/* Select which attributes we want to restore. */
	flags = ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM | ARCHIVE_EXTRACT_ACL | ARCHIVE_EXTRACT_FFLAGS;

	struct archive *a = archive_read_new();
	archive_read_support_format_all(a);
	archive_read_support_compression_all(a);

	struct archive *ext = archive_write_disk_new();
	archive_write_disk_set_options(ext, flags);
	archive_write_disk_set_standard_lookup(ext);

	if ((r = archive_read_open_filename(a, archiveFilename.c_str(), 10240)))
		return false;

	bool returnThis = true;

	try
	{
		while(true)
		{
			r = archive_read_next_header(a, &entry);
			if (r == ARCHIVE_EOF)
				break;

			if (r < ARCHIVE_OK)
				Log::log() << "Problem reading archive " << archiveFilename << ": " << archive_error_string(a) << std::endl;

			if (r < ARCHIVE_WARN)
				throw std::runtime_error("Error reading archive " + archiveFilename + ": " + archive_error_string(a));

			std::string archivedFilePath = archive_entry_pathname(entry);
			if(fileFilter(archivedFilePath))
			{
				//change directory to chosen output location
				const std::string fullOutputPath	= destination + archiveEntryPathModifier(archivedFilePath);
				archive_entry_set_pathname(entry, fullOutputPath.c_str());

				r = archive_write_header(ext, entry);

				if (r < ARCHIVE_OK)
					Log::log() << "Problem writing header from archive " << archiveFilename << ": " << archive_error_string(ext) << std::endl;
				else if (archive_entry_size(entry) > 0)
				{
					r = copy_data(a, ext);

					if (r < ARCHIVE_OK)
						Log::log() << "Problem writing data from archive " << archiveFilename << ": " << archive_error_string(ext) << std::endl;

					if (r < ARCHIVE_WARN)
						throw std::runtime_error("Error writing archive " + archiveFilename + ": " + archive_error_string(ext));
				}

				r = archive_write_finish_entry(ext);

				if (r < ARCHIVE_OK)
					Log::log() << "Problem finishing writing data from archive " << archiveFilename << ": " << archive_error_string(ext) << std::endl;

				if (r < ARCHIVE_WARN)
					throw std::runtime_error("Error finishing writing archive " + archiveFilename + ": " + archive_error_string(ext));
			}
		}
	}
	catch(std::runtime_error e)
	{
		Log::log() << e.what() << std::endl;
		returnThis = false;
	}

	archive_read_close(a);
	archive_read_free(a);
	archive_write_close(ext);
	archive_write_free(ext);

	return returnThis;
}

bool ExtractArchive::extractArchiveToFolderFlattened(std::string archiveFilename, std::string destination, const std::set<std::string> & exemptions)
{
	auto pathModifier = [&](std::string in)
	{
		std::string beforeLastSlash = in.substr(0, in.find_last_of('/'));
		std::string folder			= beforeLastSlash.substr(beforeLastSlash.find_last_of('/') + 1);
		auto slashpos				= in.find_last_of('/');
		std::string filename		= in.substr(slashpos != std::string::npos ? slashpos : 0);

		if(folder == "" || exemptions.count(folder) == 0)
			return filename;
		return folder + '/' + filename;
	};

	return _extractArchiveToFolder(archiveFilename, destination, pathModifier);
}

/*bool ExtractArchive::extractJaspModule(std::string archiveFilename, std::string destination, const std::map<std::string, std::set<std::string>> & folderAndExtensionExemptions)
{
	auto fileNameExtractor	= [&](std::string in, std::string & folder, std::string & filename)
	{
		std::string beforeLastSlash = in.substr(0, in.find_last_of('/')),
					fullPath		= beforeLastSlash.substr(beforeLastSlash.find_last_of('/') + 1);

					folder			= stringUtils::toLower(fullPath);
		auto		slashpos		= in.find_last_of('/');
					filename		= in.substr(slashpos != std::string::npos ? slashpos + 1 : 0);

		if(filename.find_last_of('.') != std::string::npos)
			return stringUtils::toLower(filename.substr(filename.find_last_of('.') + 1)); // return extension in lowercase
		return std::string("");
	};

	auto pathModifier = [&](std::string in)
	{
		std::string folder, filename;

		fileNameExtractor(in, folder, filename);

		if(folder == "" || folderAndExtensionExemptions.count(folder) == 0)
			return filename;

		if(folder == "r")
			folder = "R"; //Because we like it like that :)

		if(folder == "help")
			filename = stringUtils::toLower(filename); //To make life easier on case sensitive filesystems like linux has (HelpModel::showOrTogglePage also expects this)

		return folder + '/' + filename;
	};

	auto fileFilter = [&](std::string in)
	{
		std::string folder, filename, extension = fileNameExtractor(in, folder, filename);

		return 	(folderAndExtensionExemptions.count(folder) > 0 && folderAndExtensionExemptions.at(folder).count(extension) > 0) ||
				(folderAndExtensionExemptions.count("")		> 0 && folderAndExtensionExemptions.at("").count(extension)		> 0)  ;
	};

	return _extractArchiveToFolder(archiveFilename, destination, pathModifier, fileFilter);
}
*/

bool ExtractArchive::isFileAnArchive(std::string filename)
{
	int flags;
	int r;

	flags = ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM | ARCHIVE_EXTRACT_ACL | ARCHIVE_EXTRACT_FFLAGS;

	struct archive *a = archive_read_new();
	archive_read_support_format_all(a);
	archive_read_support_compression_all(a);

	bool itIsAnArchive = archive_read_open_filename(a, filename.c_str(), 10240) == ARCHIVE_OK;

	archive_read_close(a);
	archive_read_free(a);

	return itIsAnArchive;
}

std::string ExtractArchive::extractSingleTextFileFromArchive(std::string archiveFilename, std::string desiredTextFileName)
{
	Log::log() << "Trying to extract file '" << desiredTextFileName << "' from archive: '" << archiveFilename << "'" << std::endl;

	std::string dataFromFile = "";
	struct archive_entry *entry;
	int flags;
	int r;

	/* Select which attributes we want to restore. */
	flags = ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM | ARCHIVE_EXTRACT_ACL | ARCHIVE_EXTRACT_FFLAGS;

	struct archive *a = archive_read_new();
	archive_read_support_format_all(a);
	archive_read_support_compression_all(a);

	if ((r = archive_read_open_filename(a, archiveFilename.c_str(), 10240)))
		throw std::runtime_error("File opening failed");

	bool keepLooking = true;

	try
	{
		while(keepLooking)
		{
			r = archive_read_next_header(a, &entry);
			if (r == ARCHIVE_EOF)
				break;

			if (r < ARCHIVE_OK)
				Log::log() << "Problem reading archive " << archiveFilename << ": " << archive_error_string(a) << std::endl;

			if (r < ARCHIVE_WARN)
				throw std::runtime_error("Error reading archive " + archiveFilename + ": " + archive_error_string(a));

			std::string entryName = archive_entry_pathname(entry);

			if(entryName.size() >= desiredTextFileName.size() && entryName.substr(entryName.size() - desiredTextFileName.size()) == desiredTextFileName)
			{
				size_t minSize	= archive_entry_size(entry);
				char * buff		= new char[minSize + 1];

				std::memset(buff, NULL, minSize + 1);

				archive_read_data(a, buff, minSize);

				dataFromFile = buff;
				delete[] buff;

				keepLooking = false;
			}

		}
	}
	catch(std::runtime_error e)
	{
		Log::log() << e.what() << std::endl;
		throw std::runtime_error("There was an error reading the request file " + desiredTextFileName + " from archive " + archiveFilename + ", this error was: " + e.what());
	}

	archive_read_close(a);
	archive_read_free(a);

	return dataFromFile;
}
