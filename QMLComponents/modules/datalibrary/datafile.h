//
// Copyright (C) 2013-2025 University of Amsterdam
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

#ifndef DATAFILE_H
#define DATAFILE_H

#include "datalibraryentry.h"

namespace Modules {

/// Leaf node in DataLibrary{} — represents a single .jasp example file.
/// path is relative to the module's examples folder (or the built-in Data Library folder).
/// dataFile is the optional associated raw data file (csv/xlsx/…), same base directory.
class DataFile : public DataLibraryEntry
{
	Q_OBJECT

	Q_PROPERTY(QString path     READ path     WRITE setPath     NOTIFY pathChanged    )
	Q_PROPERTY(QString dataFile READ dataFile WRITE setDataFile NOTIFY dataFileChanged)

public:
	explicit DataFile(QQuickItem * parent = nullptr);

	const QString & path()     const { return _path;     }
	const QString & dataFile() const { return _dataFile; }

public slots:
	void setPath(    const QString & path);
	void setDataFile(const QString & dataFile);

signals:
	void pathChanged();
	void dataFileChanged();

private:
	QString _path;
	QString _dataFile;
};

} // namespace Modules

#endif // DATAFILE_H
