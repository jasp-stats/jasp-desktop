#include "autosavefilelistmodel.h"
#include "autosavefilesystem.h"

AutoSaveFileListModel::AutoSaveFileListModel(QObject *parent)
	: FileMenuBasicListModel{parent, new AutoSaveFileSystem(parent)}
{

}

void AutoSaveFileListModel::refresh()
{
	beginResetModel();
	_model->refresh();
	endResetModel();
}

void AutoSaveFileListModel::openFile(const QString &path)
{
	if (path.isEmpty())
		return;

	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileOpen);
	event->setPath(path);

	emit dataSetIORequest(event);
}
