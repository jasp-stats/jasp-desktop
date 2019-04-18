#ifndef CURRENTFILELISTMODEL_H
#define CURRENTFILELISTMODEL_H

#include <QAbstractListModel>
#include "currentfilefilesystem.h"
#include "data/fileevent.h"
#include "filemenulistitem.h"
#include "filemenubasiclistmodel.h"

class CurrentFileListModel  : public FileMenuBasicListModel
{
	Q_OBJECT
	
public:
	explicit CurrentFileListModel(QObject *parent = nullptr);
	
	CurrentFileFileSystem*		getCurrentFileFSBModel();
	void					setCurrentFilePath(const QString &newcurrent);
	
signals:
	void syncCurrentFile(FileEvent *event);

public slots:
	void openFile(const QString& path) override;

	
	
private:
	CurrentFileFileSystem			*_fsbmCurrentFile;
};

#endif // CURRENTFILELISTMODEL_H
