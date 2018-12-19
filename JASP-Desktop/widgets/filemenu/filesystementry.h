//
// Copyright (C) 2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef FILESYSTEMENTRY_H
#define FILESYSTEMENTRY_H

#include <QString>
#include "utils.h"
#include <QHash>

class FileSystemEntry
{
public:
	enum EntryType { JASP = 0, CSV = 1, SPSS = 2, Folder = 3, Other = 4, NoOfTypes = 5 };

	FileSystemEntry() { entryType = Other; }

	QString	name,
			path,
			description,
			associated_datafile = "";

	EntryType entryType;
	
	static inline EntryType getEntryTypeFromPath(const QString &path)
	{
		Utils::FileType basefiletype = Utils::getTypeFromFileName(path.toStdString());

		switch (basefiletype)
		{
		case Utils::FileType::csv:		return FileSystemEntry::CSV;
		case Utils::FileType::jasp:		return FileSystemEntry::JASP;
		case Utils::FileType::sav:		return FileSystemEntry::SPSS;
		case Utils::FileType::unknown:	return FileSystemEntry::NoOfTypes;
		default:						return FileSystemEntry::Other;
		}
	}

	static QHash<int, QString> sourcesIcons()
	{
		static QHash<int, QString> icons = {
			{ FileSystemEntry::JASP,	"qrc:/icons/file-jasp.svg"		},
			{ FileSystemEntry::CSV,		"qrc:/icons/spreadsheet.svg"	},
			{ FileSystemEntry::SPSS,	"qrc:/icons/spreadsheet.svg"	},
			{ FileSystemEntry::Other,	"qrc:/icons/spreadsheet.svg"	},
			{ FileSystemEntry::Folder,	"qrc:/icons/folder.svg"			} };

		return icons;
	}

};

#endif // FILESYSTEMENTRY_H
