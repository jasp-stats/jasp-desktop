//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>
#include "sysdepfiletype.h"

class Utils
{
public:
	enum FileType { jasp = 0, html, csv, txt, sav, ods, pdf, empty, unknown };
	typedef std::vector<Utils::FileType> FileTypeVector;

	static const char* getFileTypeString(const Utils::FileType &fileType);
	static Utils::FileType getTypeFromFileName(const JaspFileTypes::FilePath &path);

	static long currentMillis();
	static long currentSeconds();
	static long getFileModificationTime(const JaspFileTypes::FilePath &filename);
	static long getFileSize(const JaspFileTypes::FilePath &filename);
	static void touch(const JaspFileTypes::FilePath &filename);
	static bool renameOverwrite(const JaspFileTypes::FilePath &oldName, const JaspFileTypes::FilePath &newName);
	static bool removeFile(const JaspFileTypes::FilePath &path);

/*	static SystemDepFileTypes::FilePath osPath(const std::string &path);
	static std::string osPath(const boost::filesystem::path &path); */

	/**
	 * \brief Removes entries int toRemove from target where filenames match.
	 * \param target The taget for removals.
	 * \param filesToRemove File name of the items to remove.
	 */
	static void remove(std::vector<JaspFileTypes::FilePath> &target, const std::vector<JaspFileTypes::FilePath> &filesToRemove);
	static void sleep(int ms);


	/**
	 * @brief deleteList Attempts to delete all the files mentioned.
	 * @param files A vector of files to delete. The file paths are assumed to be complete.
	 */
	static void deleteListFullPaths(const std::vector<JaspFileTypes::FilePath> &files);
};

/*
 * Implememetation of Utils::remove.
template<class Item>
void Utils::remove(std::vector<Item> &target, const std::vector<Item> &toRemove)
{
	for (typename std::vector<Item>::const_iterator removeIter = toRemove.begin(); removeIter != toRemove.end(); ++removeIter)
	{
		typename std::vector<Item>::iterator targetItr = target.begin();
		while (targetItr  != target.end())
		{
			if (*targetItr == *removeIter)
				target.erase(targetItr);
			else
				targetItr++;
		}
	}
} */


#endif // UTILS_H
