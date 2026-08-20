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

#ifndef DATALIBRARYDESCRIPTION_H
#define DATALIBRARYDESCRIPTION_H

#include <QTimer>
#include <QQuickItem>
#include "datalibraryentry.h"

namespace Modules {

/// Root element of DataLibrary.qml — the QML type is registered as "DataLibrary".
/// Mirrors the Description class: children register themselves via parentChanged,
/// and a debounce timer batches change notifications into iShouldBeUpdated.
class DataLibraryDescription : public QQuickItem
{
	Q_OBJECT

public:
	explicit DataLibraryDescription(QQuickItem * parent = nullptr);

	void addChild(   DataLibraryEntry * entry);
	void removeChild(DataLibraryEntry * entry);

	const QList<DataLibraryEntry *> & entries() const { return _entries; }

	/// Display title used as the top-level folder name in the Data Library.
	/// Set by DynamicModule after loading (module title), or left empty for the built-in library.
	const QString & moduleTitle() const         { return _moduleTitle; }
	void            setModuleTitle(const QString & title) { _moduleTitle = title; }

	/// The built-in library description, loaded from Resources/Data Sets/DataLibrary.qml.
	static DataLibraryDescription * builtIn()                              { return _builtIn; }
	static void                     setBuiltIn(DataLibraryDescription * d) { _builtIn = d;    }

signals:
	void iShouldBeUpdated(DataLibraryDescription * desc);

private slots:
	void delayedUpdate();

private:
	QList<DataLibraryEntry *>  _entries;
	QString                    _moduleTitle;
	QTimer                     _timer;

	static DataLibraryDescription * _builtIn;
};

} // namespace Modules

#endif // DATALIBRARYDESCRIPTION_H
