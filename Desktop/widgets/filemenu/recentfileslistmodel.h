#ifndef RECENTFILESLISTMODEL_H
#define RECENTFILESLISTMODEL_H

#include <QAbstractListModel>
#include "data/fileevent.h"
#include "recentfilesfilesystem.h"
#include "filemenulistitem.h"
#include "filemenubasiclistmodel.h"

class RecentFilesListModel : public FileMenuBasicListModel
{
	Q_OBJECT

public:
	explicit RecentFilesListModel(QObject *parent = nullptr);

	void addRecentFilePath(const QString &newpath);

signals:
	void openFileEvent(FileEvent *event);

public slots:
	void openFile(const QString& path) override;

private:
	RecentFilesFileSystem *_fsbmRecentFiles;
};

#endif // RECENTFILESLISTMODEL_H
