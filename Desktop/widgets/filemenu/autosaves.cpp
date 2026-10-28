#include "autosaves.h"


AutoSaves::AutoSaves(FileMenu *parent)
	: FileMenuObject(parent)
	, _listModel(new AutoSaveFileListModel(this))
{
}
