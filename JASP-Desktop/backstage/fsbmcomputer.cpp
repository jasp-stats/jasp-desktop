
#include "fsbmcomputer.h"

#include <QStandardPaths>
#include <QDir>

#include "fsentry.h"

FSBMComputer::FSBMComputer()
{
	_rootPath = _path = QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).at(0);
}

void FSBMComputer::refresh()
{
	QStringList filterDataFiles;
	filterDataFiles.append("*.jasp");
	filterDataFiles.append("*.csv");

	QDir dir(_path);

	QStringList folders   = dir.entryList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name | QDir::IgnoreCase);
	QStringList dataFiles = dir.entryList(filterDataFiles, QDir::Files, QDir::Name | QDir::IgnoreCase);

	_entries.clear();

	foreach (const QString &folder, folders)
	{
		QString path = _path + "/" + folder;

		_entries.append(createEntry(path, FSEntry::Folder));
	}

	foreach (const QString &file, dataFiles)
	{
		QString path = _path + "/" + file;

		if (file.endsWith(".jasp", Qt::CaseInsensitive))
			_entries.append(createEntry(path, FSEntry::JASP));
		else
			_entries.append(createEntry(path));
	}

	emit entriesChanged();
}

