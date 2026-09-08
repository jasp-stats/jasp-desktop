#include "autosavefilelistmodel.h"
#include "autosavefilesystem.h"

AutoSaveFileListModel::AutoSaveFileListModel(FileMenuObject *parent)
	: FileMenuBasicListModel{parent, new AutoSaveFileSystem(parent)}
{

}

void AutoSaveFileListModel::refresh()
{
	beginResetModel();
	_model->refresh();
	endResetModel();
}
