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

#ifndef DATALIBRARYENTRY_H
#define DATALIBRARYENTRY_H

#include <QQuickItem>

namespace Modules {

class DataFolder;
class DataLibraryDescription;

/// Base QML item for data library entries (DataFile and DataFolder).
/// Automatically registers with its parent (DataLibraryDescription or DataFolder)
/// via the parentChanged signal, mirroring the DescriptionChildBase pattern.
class DataLibraryEntry : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY(QString name        READ name        WRITE setName        NOTIFY nameChanged       )
	Q_PROPERTY(QString description READ description WRITE setDescription NOTIFY descriptionChanged)

public:
	explicit DataLibraryEntry(QQuickItem * parent = nullptr);

	const QString & name()        const { return _name;        }
	const QString & description() const { return _description; }

public slots:
	void setName(       const QString & name);
	void setDescription(const QString & desc);

signals:
	void nameChanged();
	void descriptionChanged();
	void somethingChanged();

private slots:
	void registerParent(QQuickItem * newParent);

private:
	QString                  _name;
	QString                  _description;
	DataLibraryDescription * _parentDescription = nullptr;
	DataFolder             * _parentFolder       = nullptr;
};

} // namespace Modules

#endif // DATALIBRARYENTRY_H
