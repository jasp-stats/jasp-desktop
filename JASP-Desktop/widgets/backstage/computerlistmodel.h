#ifndef COMPUTERLISTMODEL_H
#define COMPUTERLISTMODEL_H

#include <QAbstractListModel>
#include "data/fileevent.h"
#include "fsbmrecentfolders.h"
#include "filemenulistitem.h"
#include "basiclistmodel.h"

class ComputerListModel : public FileMenuBasicListModel
{
	Q_OBJECT

public:
	explicit ComputerListModel(QObject *parent = nullptr);

	QString					getMostRecent();
	void					addRecentFolder(const QString &newpath);
	void					refresh();

public slots:
	void changePath(const QString& name, const QString& path) override { emit browseOpen(path); }

signals:
	void browseOpen(const QString & path);
					
private:
	FSBMRecentFolders *_fsbmRecentFolders;
};

#endif // COMPUTERLISTMODEL_H
