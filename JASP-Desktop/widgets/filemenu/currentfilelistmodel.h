#ifndef CURRENTFILELISTMODEL_H
#define CURRENTFILELISTMODEL_H

#include <QAbstractListModel>
#include "fsbmcurrentfile.h"
#include "data/fileevent.h"
#include "filemenulistitem.h"
#include "basiclistmodel.h"

class CurrentFileListModel  : public FileMenuBasicListModel
{
	Q_OBJECT
	
public:
	explicit CurrentFileListModel(QObject *parent = nullptr);
	
	FSBMCurrentFile*		getCurrentFileFSBModel();
	void					setCurrentFilePath(const QString &newcurrent);
	
signals:
	void syncFile(FileEvent *event);

public slots:
	void syncFile(const QString& path);	
	
	
private:
	FSBMCurrentFile			*_fsbmCurrentFile;
};

#endif // CURRENTFILELISTMODEL_H
