#ifndef RECENTFILESLISTMODEL_H
#define RECENTFILESLISTMODEL_H

#include <QAbstractListModel>
#include "data/fileevent.h"
#include "fsbmrecentfiles.h"
#include "filemenulistitem.h"
#include "basiclistmodel.h"

class RecentFilesListModel : public FileMenuBasicListModel
{
	Q_OBJECT

public:
	explicit RecentFilesListModel(QObject *parent = nullptr);

	void addRecentFilePath(const QString &newpath);

signals:
	void openFile(FileEvent *event);

public slots:
	void openFile(const QString& path) override;

private:
	FSBMRecentFiles *_fsbmRecentFiles;
};

#endif // RECENTFILESLISTMODEL_H
