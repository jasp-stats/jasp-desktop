#ifndef FSBMODEL_H
#define FSBMODEL_H

#include <QObject>
#include <QList>
#include <QStringList>

#include "fsentry.h"

class FSBModel : public QObject
{
	Q_OBJECT

	friend class FileSystemTableModel;

public:
	explicit FSBModel(QObject *parent = NULL);
	typedef QList<FSEntry> FileSystemEntryList;

	virtual void refresh() = 0;

	const FileSystemEntryList &entries() const;

	const QString &path() const;
	const QString &rootPath() const;

public slots:
	void setPath(QString path);

signals:
	void entriesChanged();
	void pathChanged(QString path);

public slots:

protected:

	QString _rootPath;
	QString _path;

	static FSEntry createEntry(const QString &path, FSEntry::EntryType type = FSEntry::Other);
	static FSEntry createEntry(const QString &path, const QString &name, const QString &description, FSEntry::EntryType type = FSEntry::Other);

	FileSystemEntryList _entries;

};

#endif // FSBMODEL_H
