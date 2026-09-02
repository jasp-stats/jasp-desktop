#ifndef CURRENTFILELISTMODEL_H
#define CURRENTFILELISTMODEL_H

#include <QAbstractListModel>
#include "currentfilefilesystem.h"
#include "filemenubasiclistmodel.h"

class CurrentFileListModel  : public FileMenuBasicListModel
{
	Q_OBJECT
	
public:
    explicit CurrentFileListModel(FileMenuObject *parent);
	
	CurrentFileFileSystem*		getCurrentFileFSBModel();
	void						setCurrentFilePath(const QString &newcurrent);
	
signals:
	void syncCurrentFile(const QString& currentFile);
	
private:
	CurrentFileFileSystem			*_fsbmCurrentFile;
};

#endif // CURRENTFILELISTMODEL_H
