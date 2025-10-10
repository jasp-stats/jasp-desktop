#ifndef AUTOSAVEFILESYSTEM_H
#define AUTOSAVEFILESYSTEM_H

#include "filesystem.h"

class AutoSaveFileSystem : public FileSystem
{
	Q_OBJECT
public:
	explicit AutoSaveFileSystem(QObject *parent = nullptr);

	void refresh() override;
};

#endif // AUTOSAVEFILESYSTEM_H
