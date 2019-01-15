//
// Copyright (C) 2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.

#pragma once

#ifndef FILEMENU_H
#define FILEMENU_H

#include <QObject>
#include <QFileSystemWatcher>
#include <QVector>

#include "widgets/filemenu/recentfiles.h"
#include "widgets/filemenu/currentfile.h"
#include "widgets/filemenu/computer.h"
#include "widgets/filemenu/osf.h"
#include "widgets/filemenu/datalibrary.h"

#include "data/fileevent.h"
#include "filemenulistitem.h"
#include "actionbuttons.h"
#include "resourcebuttonsvisible.h"

class MainWindow;

class FileMenu : public QObject
{
	Q_OBJECT

	typedef ActionButtons::FileOperation FileOperation;
	
	Q_PROPERTY(FileOperation fileoperation				READ fileoperation				WRITE setFileoperation				NOTIFY fileoperationChanged)

	Q_PROPERTY(DataLibrary *  datalibrary				READ datalibrary				WRITE setDatalibrary				NOTIFY datalibraryChanged)
	Q_PROPERTY(CurrentFile * currentFile				READ currentFile				WRITE setCurrentFile				NOTIFY currentFileChanged)
	Q_PROPERTY(RecentFiles * recentFiles				READ recentFiles				WRITE setRecentFiles				NOTIFY recentFilesChanged)
	Q_PROPERTY(Computer * computer						READ computer					WRITE setComputer					NOTIFY computerChanged)
	Q_PROPERTY(OSF * osf								READ osf						WRITE setOsf						NOTIFY osfChanged)

	Q_PROPERTY(bool visible								READ visible					WRITE setVisible					NOTIFY visibleChanged)

	Q_PROPERTY(ActionButtons * actionButtons			READ actionButtons													NOTIFY actionButtonsChanged)
	Q_PROPERTY(ResourceButtonsVisible * resourceButtons	READ resourceButtons												NOTIFY resourceButtonsChanged)

public:
	enum FileLocation { Recent = 0, Current, ThisComputer, Osf, Examples, CountLocations };

	Q_ENUM(FileMenuListItemType)
	
	explicit FileMenu(QObject *parent = nullptr);
	virtual ~FileMenu() {}
	
	void			setFileoperation(const FileOperation fo);

	void		setOnlineDataManager(OnlineDataManager *odm);
	FileEvent *	open(const QString &filepath);
	FileEvent *	save();
	void		sync();
	FileEvent *	close();
			
	void			setCurrentDataFile(const QString &path);
	void			setDataFileWatcher(bool watch);
	
	void			setSaveMode(FileEvent::FileMode mode);
	Utils::FileType getCurrentFileType();
	QString			getCurrentFilePath();
	QString			getDefaultOutFileName();
	bool			isCurrentFileReadOnly();

	FileOperation				fileoperation()			const	{ return _fileoperation;			}
	DataLibrary *				datalibrary()			const	{ return _DataLibrary;				}
	CurrentFile *				currentFile()			const	{ return _CurrentFile;				}
	RecentFiles *				recentFiles()			const	{ return _RecentFiles;				}
	Computer *					computer()				const	{ return _Computer;					}
	OSF *						osf()					const	{ return _OSF;						}
	bool						visible()				const	{ return _visible;					}
	ActionButtons *				actionButtons()			const	{ return _actionButtons;			}
	ResourceButtonsVisible *	resourceButtons()		const	{ return _resourceButtonsVisible;	}

signals:
	void fileoperationChanged();
	void dataSetIORequest(FileEvent *event);
	void exportSelected(QString filename);
	void datalibraryChanged(DataLibrary * datalibrary);
	void currentFileChanged(CurrentFile * currentFile);
	void recentFilesChanged(RecentFiles * recentFiles);
	void computerChanged(Computer * computer);
	void osfChanged(OSF * osf);
	void visibleChanged(bool visible);
	void actionButtonsChanged(ActionButtons * actionButtons);
	void resourceButtonsChanged(ResourceButtonsVisible * resourceButtons);

public slots:
	void analysisAdded(Analysis *analysis);
	void setSyncFile(FileEvent *event);
	void dataAutoSynchronizationChanged(bool on);
	void dataSetIOCompleted(FileEvent *event);
	void dataFileModifiedHandler(QString path);
	void fileOperationClicked(const FileOperation action);
	void setDatalibrary(DataLibrary * datalibrary);
	void setCurrentFile(CurrentFile * currentFile);
	void setRecentFiles(RecentFiles * recentFiles);
	void setComputer(Computer * computer);
	void setOsf(OSF * osf);
	void setVisible(bool visible);
	void showFileMenu()	{ setVisible(true); }
	void resourceButtonClicked(const int buttonType);

private slots:
	void dataSetOpenRequestHandler(QString path);
	void dataSetOpenCurrentRequestHandler(QString path);
	void dataSetIORequestHandler(FileEvent *event);
	void dataSetOpenExampleRequestHandler(QString path);

private:
			bool checkSyncFileExists(const QString &path);
			void clearSyncData();
	static	bool clearOSFFromRecentList(QString path);

private:
	OnlineDataManager		*_odm						= nullptr;
	CurrentFile				*_CurrentFile				= nullptr;
	RecentFiles				*_RecentFiles				= nullptr;
	Computer				*_Computer					= nullptr;
	OSF						*_OSF						= nullptr;
	DataLibrary				*_DataLibrary				= nullptr;
	ActionButtons			*_actionButtons				= nullptr;
	ResourceButtons			*_resourceButtons			= nullptr;
	ResourceButtonsVisible	*_resourceButtonsVisible	= nullptr;

	FileEvent::FileMode _mode;
	QString				_currentFilePath;
	Utils::FileType		_currentFileType;
	bool				_currentFileReadOnly;
	QFileSystemWatcher	_watcher;
	bool				_visible = false;
	FileOperation		_fileoperation = ActionButtons::Open;
};

#endif // FILEMENU_H
