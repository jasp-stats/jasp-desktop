#ifndef AUTOSAVEFILELISTMODEL_H
#define AUTOSAVEFILELISTMODEL_H

#include "filemenubasiclistmodel.h"

class AutoSaveFileListModel : public FileMenuBasicListModel
{
	Q_OBJECT
public:
	explicit			AutoSaveFileListModel(QObject *parent = nullptr);

	Q_INVOKABLE void	refresh();
				void	openFile(const QString &path) override;

signals:
				void	dataSetIORequest(FileEvent *event);
};

#endif // AUTOSAVEFILELISTMODEL_H
