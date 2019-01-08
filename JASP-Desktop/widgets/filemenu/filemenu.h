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

class MainWindow;

class FileMenu : public QObject
{
	Q_OBJECT
	
	Q_PROPERTY(	bool enable_currentfile_button		READ enable_currentfile_button	WRITE set_enable_currentfile_button	NOTIFY enable_currentfile_button_changed) //Is this the same as current_button_enable???
	Q_PROPERTY(	FileOperation fileoperation			READ fileoperation				WRITE setFileoperation				NOTIFY fileoperationChanged)
	Q_PROPERTY(	QVector<bool> buttonsenabled		READ buttonsenabled				WRITE setButtonsenabled				NOTIFY buttonsenabledChanged)

	Q_PROPERTY(DataLibrary *  datalibrary			READ datalibrary				WRITE setDatalibrary				NOTIFY datalibraryChanged)
	Q_PROPERTY(CurrentFile * currentFile			READ currentFile				WRITE setCurrentFile				NOTIFY currentFileChanged)
	Q_PROPERTY(RecentFiles * recentFiles			READ recentFiles				WRITE setRecentFiles				NOTIFY recentFilesChanged)
	Q_PROPERTY(Computer * computer					READ computer					WRITE setComputer					NOTIFY computerChanged)
	Q_PROPERTY(OSF * osf							READ osf						WRITE setOsf						NOTIFY osfChanged)

	Q_PROPERTY(bool recentfiles_button_visible		READ recentfiles_button_visible	WRITE setRecentfiles_button_visible NOTIFY recentfiles_button_visibleChanged)
	Q_PROPERTY(bool currentfile_button_visible		READ currentfile_button_visible	WRITE setCurrentfile_button_visible NOTIFY currentfile_button_visibleChanged)
	Q_PROPERTY(bool computer_button_visible			READ computer_button_visible	WRITE setComputer_button_visible	NOTIFY computer_button_visibleChanged)
	Q_PROPERTY(bool datalibrary_button_visible		READ datalibrary_button_visible	WRITE setDatalibrary_button_visible NOTIFY datalibrary_button_visibleChanged)

	Q_PROPERTY(bool visible							READ visible					WRITE setVisible					NOTIFY visibleChanged)

public:
	
	enum FileOperation {Open = 0, Save, SaveAs, ExportResults, ExportData, SyncData, Close, CountFileActions};
	Q_ENUM(FileOperation)
	enum FileLocation {Recent = 0, Current, ThisComputer, Osf, Examples, CountLocations};


	Q_ENUM(FileMenuListItemType)
	
	explicit FileMenu(QObject *parent = nullptr);
	virtual ~FileMenu() {}
	
	bool enable_currentfile_button();

	FileOperation fileoperation();
	QVector<bool>  buttonsenabled();

	void set_enable_currentfile_button(const bool enable);
	void setFileoperation(const FileMenu::FileOperation fo);

	void setButtonsenabled(const QVector<bool> &enable);
	
	//Redirected
	void setOnlineDataManager(OnlineDataManager *odm);
	FileEvent *open(const QString &filepath);
	FileEvent *save();
	void sync();
	FileEvent *close();
		
	//Backstage
	void setCurrentActionIndex(int index);
			
	//OpenAndSave
	void setCurrentDataFile(const QString &path);
	void setDataFileWatcher(bool watch);
	
	void setSaveMode(FileEvent::FileMode mode);	
	Utils::FileType getCurrentFileType();
	QString getCurrentFilePath();
	QString getDefaultOutFileName();
	bool isCurrentFileReadOnly();

	DataLibrary * datalibrary()			const	{ return _DataLibrary;	}
	CurrentFile * currentFile()			const	{ return _CurrentFile;	}
	RecentFiles * recentFiles()			const	{ return _RecentFiles;	}
	Computer * computer()				const	{ return _Computer;		}
	OSF * osf()							const	{ return _OSF;			}
	
	bool recentfiles_button_visible()	const	{ return m_recentfiles_button_visible;	}
	bool currentfile_button_visible()	const	{ return m_currentfile_button_visible;	}
	bool computer_button_visible()		const	{ return m_computer_button_visible;		}
	bool datalibrary_button_visible()	const	{ return m_datalibrary_button_visible;	}

	bool visible()						const	{ return m_visible;}

signals:
	
	void enable_currentfile_button_changed();
	void fileoperationChanged();
	void buttonsenabledChanged();
	
	//Backstage
	void dataSetIORequest(FileEvent *event);
	void exportSelected(QString filename);

	void datalibraryChanged(DataLibrary * datalibrary);
	void currentFileChanged(CurrentFile * currentFile);
	void recentFilesChanged(RecentFiles * recentFiles);
	void computerChanged(Computer * computer);
	void osfChanged(OSF * osf);

	void recentfiles_button_visibleChanged(bool recentfiles_button_visible);
	void currentfile_button_visibleChanged(bool currentfile_button_visible);
	void computer_button_visibleChanged(bool computer_button_visible);
	void datalibrary_button_visibleChanged(bool datalibrary_button_visible);

	void visibleChanged(bool visible);

public slots:
	//Backstage
	void analysisAdded(Analysis *analysis);
	
	//Redirected
	void setSyncFile(FileEvent *event);
	void dataAutoSynchronizationChanged(bool on);
	
	void dataSetIOCompleted(FileEvent *event);
	void dataFileModifiedHandler(QString path);
	
	void fileOperationClicked(const int &action);
	void resourceButtonClicked(const int &resource);
	
	void test();

	void setDatalibrary(DataLibrary * datalibrary);
	void setCurrentFile(CurrentFile * currentFile);
	void setRecentFiles(RecentFiles * recentFiles);
	void setComputer(Computer * computer);
	void setOsf(OSF * osf);

	void setRecentfiles_button_visible(bool recentfiles_button_visible);
	void setCurrentfile_button_visible(bool currentfile_button_visible);
	void setComputer_button_visible(bool computer_button_visible);
	void setDatalibrary_button_visible(bool datalibrary_button_visible);
	void setVisible(bool visible);
	void showFileMenu()	{ setVisible(true); }

private slots:
	void dataSetOpenRequestHandler(QString path);
	void dataSetOpenCurrentRequestHandler(QString path);
	void dataSetIORequestHandler(FileEvent *event);
	void dataSetOpenExampleRequestHandler(QString path);

private:
	//OpenAndSave
	bool checkSyncFileExists(const QString &path);
	void clearSyncData();
	static bool clearOSFFromRecentList(QString path);
	void setEnableButton(const int &index, const bool &enable);
	
	OnlineDataManager *_odm = nullptr;
	FileEvent::FileMode _mode;
	
	QString _currentFilePath;
	Utils::FileType _currentFileType;
	bool _currentFileReadOnly;
	int _selectedActionIndex;
	int _selectedResourceIndex;
	
	CurrentFile				*_CurrentFile	= nullptr;
	RecentFiles	*_RecentFiles	= nullptr;
	Computer		*_Computer		= nullptr;
	OSF						*_OSF			= nullptr;
	DataLibrary				*_DataLibrary	= nullptr;

	QFileSystemWatcher _watcher;	
			
	bool _enable_currentfile_button;

	FileOperation _fileoperation = Close;
	QVector<bool> _buttonsenabled;

	bool m_recentfiles_button_visible;
	bool m_currentfile_button_visible;
	bool m_computer_button_visible;
	bool m_datalibrary_button_visible;
	bool m_visible = false;
};

#endif // FILEMENU_H
