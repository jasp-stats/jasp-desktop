
#include "fsbmodel.h"

FSBModel::FSBModel(QObject *parent) : QObject(parent)
{

}

const FSBModel::FileSystemEntryList &FSBModel::entries() const
{
	return _entries;
}

void FSBModel::setPath(QString path)
{
	_path = path;

	refresh();

	emit pathChanged(path);
}

const QString &FSBModel::path() const
{
	return _path;
}

const QString &FSBModel::rootPath() const
{
	return _rootPath;
}

FSEntry FSBModel::createEntry(const QString &path, FSEntry::EntryType type)
{
	FSEntry entry;
	entry.entryType = type;

	int index = path.lastIndexOf("/");
	if (index != -1)
	{
		entry.name = path.mid(index + 1);
		entry.path = path;
		entry.description = path.mid(0, index);
	}
	else
	{
		entry.name = path;
		entry.path = path;
		entry.description = "";
	}

	return entry;
}

FSEntry FSBModel::createEntry(const QString &path, const QString &name, const QString &description, FSEntry::EntryType type)
{
	FSEntry entry;

	entry.name = name;
	entry.path = path;
	entry.description = description;
	entry.entryType = type;

	return entry;
}
