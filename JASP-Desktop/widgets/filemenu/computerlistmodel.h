#ifndef COMPUTERLISTMODEL_H
#define COMPUTERLISTMODEL_H

#include <QAbstractListModel>
#include "data/fileevent.h"
#include "computerfilesystem.h"
#include "filemenulistitem.h"
#include "filemenubasiclistmodel.h"

class ComputerListModel : public FileMenuBasicListModel
{
	Q_OBJECT

public:
	explicit ComputerListModel(QObject *parent = nullptr);

	QString					getMostRecent();
	void					addRecentFolder(const QString &newpath);
	void					refresh();

public slots:
	void changePath(const QString& name, const QString& path) override { emit browsePath(path); }

signals:
	void browsePath(const QString & path);
					
private:
	ComputerFileSystem *_fsbmRecentFolders;
};

#endif // COMPUTERLISTMODEL_H
