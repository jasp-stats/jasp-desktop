#ifndef AUTOSAVEFILELISTMODEL_H
#define AUTOSAVEFILELISTMODEL_H

#include "filemenubasiclistmodel.h"

class AutoSaveFileListModel : public FileMenuBasicListModel
{
	Q_OBJECT
public:
	explicit			AutoSaveFileListModel(FileMenuObject *parent);

	Q_INVOKABLE void	refresh();
};

#endif // AUTOSAVEFILELISTMODEL_H
