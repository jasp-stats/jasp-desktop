#ifndef AUTOSAVES_H
#define AUTOSAVES_H

#include "filemenuobject.h"
#include "autosavefilelistmodel.h"

class AutoSaves : public FileMenuObject
{
	Q_OBJECT

	Q_PROPERTY(AutoSaveFileListModel * listModel READ listModel CONSTANT)

public:
	AutoSaves(FileMenu *parent);

	AutoSaveFileListModel * listModel() { return _listModel; }

private:
	AutoSaveFileListModel	*	_listModel = nullptr;
};

#endif // AUTOSAVES_H
