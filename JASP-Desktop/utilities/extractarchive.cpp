#include "extractarchive.h"
#include <iostream>
#include <sstream>

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
			std::cerr << "Problem copying data in archive: " << archive_error_string(aw) << std::endl;
			return (r);
		}
	}
}



bool ExtractArchive::_extractArchiveToFolder(std::string archiveFilename, std::string destination, std::function<std::string(std::string)> archiveEntryPathModifier)
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
				std::cerr << "Problem reading archive " << archiveFilename << ": " << archive_error_string(a) << std::endl;

			if (r < ARCHIVE_WARN)
				throw std::runtime_error("Error reading archive " + archiveFilename + ": " + archive_error_string(a));

			//change directory to chosen output location
			const std::string fullOutputPath	= destination + archiveEntryPathModifier(archive_entry_pathname(entry));
			archive_entry_set_pathname(entry, fullOutputPath.c_str());

			r = archive_write_header(ext, entry);

			if (r < ARCHIVE_OK)
				std::cerr << "Problem writing header from archive " << archiveFilename << ": " << archive_error_string(ext) << std::endl;
			else if (archive_entry_size(entry) > 0)
			{
				r = copy_data(a, ext);

				if (r < ARCHIVE_OK)
					std::cerr << "Problem writing data from archive " << archiveFilename << ": " << archive_error_string(ext) << std::endl;

				if (r < ARCHIVE_WARN)
					throw std::runtime_error("Error writing archive " + archiveFilename + ": " + archive_error_string(ext));
			}

			r = archive_write_finish_entry(ext);

			if (r < ARCHIVE_OK)
				std::cerr << "Problem finishing writing data from archive " << archiveFilename << ": " << archive_error_string(ext) << std::endl;

			if (r < ARCHIVE_WARN)
				throw std::runtime_error("Error finishing writing archive " + archiveFilename + ": " + archive_error_string(ext));
		}
	}
	catch(std::runtime_error e)
	{
		std::cerr << e.what() << std::endl;
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
	std::string dataFromFile = "File not found";
	struct archive_entry *entry;
	int flags;
	int r;

	/* Select which attributes we want to restore. */
	flags = ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM | ARCHIVE_EXTRACT_ACL | ARCHIVE_EXTRACT_FFLAGS;

	struct archive *a = archive_read_new();
	archive_read_support_format_all(a);
	archive_read_support_compression_all(a);

	if ((r = archive_read_open_filename(a, archiveFilename.c_str(), 10240)))
		return "File opening failed";

	bool keepLooking = true;

	try
	{
		while(keepLooking)
		{
			r = archive_read_next_header(a, &entry);
			if (r == ARCHIVE_EOF)
				break;

			if (r < ARCHIVE_OK)
				std::cerr << "Problem reading archive " << archiveFilename << ": " << archive_error_string(a) << std::endl;

			if (r < ARCHIVE_WARN)
				throw std::runtime_error("Error reading archive " + archiveFilename + ": " + archive_error_string(a));

			std::string entryName = archive_entry_pathname(entry);

			if(entryName.size() >= desiredTextFileName.size() && entryName.substr(entryName.size() - desiredTextFileName.size()) == desiredTextFileName)
			{
				size_t minSize	= archive_entry_size(entry);
				char * buff		= new char[minSize + 1];

				memset(buff, NULL, minSize + 1);

				archive_read_data(a, buff, minSize);

				dataFromFile = buff;
				delete[] buff;

				keepLooking = false;
			}

		}
	}
	catch(std::runtime_error e)
	{
		std::cerr << e.what() << std::endl;
		dataFromFile = "There was an error reading the request file " + desiredTextFileName + " from archive " + archiveFilename + ", this error was: " + e.what();
	}

	archive_read_close(a);
	archive_read_free(a);

	return dataFromFile;
}
