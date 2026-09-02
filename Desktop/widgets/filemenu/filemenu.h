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
#include "widgets/filemenu/currentdatafile.h"
#include "widgets/filemenu/autosaves.h"
#include "widgets/filemenu/computer.h"
#include "widgets/filemenu/osf.h"
#include "widgets/filemenu/databasefilemenu.h"
#include "widgets/filemenu/datalibrary.h"

#include "data/fileevent.h"
#include "data/fileeventrouter.h"
#include "filemenulistitem.h"
#include "actionbuttons.h"
#include "resourcebuttonsvisible.h"
#include "analysis/analysis.h"


class MainWindow;
class DataSetPackage;

class FileMenu : public FileEventRouter
{
	friend FileMenuObject;

	typedef ActionButtons::FileOperation FileOperation;
	Q_OBJECT

	Q_PROPERTY(FileOperation				fileoperation			READ fileoperation				WRITE setFileoperation				NOTIFY fileoperationChanged		)
	Q_PROPERTY(DataLibrary				*	datalibrary				READ datalibrary													CONSTANT						)
	Q_PROPERTY(CurrentDataFile			*	currentFile				READ currentFile													CONSTANT						)
	Q_PROPERTY(RecentFiles				*	recentFiles				READ recentFiles													CONSTANT						)
	Q_PROPERTY(AutoSaves				*	autoSaves				READ autoSaves														CONSTANT						)
	Q_PROPERTY(Computer					*	computer				READ computer														CONSTANT						)
	Q_PROPERTY(OSF						*	osf						READ osf															CONSTANT						)
	Q_PROPERTY(DatabaseFileMenu			*	database				READ database														CONSTANT						)
	Q_PROPERTY(ActionButtons			*	actionButtons			READ actionButtons													CONSTANT						)
	Q_PROPERTY(ResourceButtons			*	resourceButtons			READ resourceButtons												CONSTANT						)
	Q_PROPERTY(ResourceButtonsVisible	*	resourceButtonsVisible	READ resourceButtonsVisible											CONSTANT						)
	Q_PROPERTY(bool							visible					READ visible					WRITE setVisible					NOTIFY visibleChanged			)

public:

	enum FileLocation { Recent = 0, Current, ThisComputer, Osf, Examples, AutoSavesLoc, CountLocations };

	Q_ENUM(FileMenuListItemType)

	explicit FileMenu(QObject *parent = nullptr);
	virtual ~FileMenu() override;

	void		setResourceButtonsVisibleFor(FileOperation fo);

	void		setOnlineDataManager(OnlineDataManager *odm);
	FileEvent * newData();
	FileEvent *	open(const QString &filepath);
	FileEvent * open(const Json::Value & databaseInfo);
	FileEvent *	save();
	FileEvent *	saveAs();
	void		sync();

	void			setCurrentDataFile(const QString		& path);
	void			setDataFileWatcher(bool watch);

	void			setMode(FileEvent::FileMode mode);
	Utils::FileType getCurrentFileType()	const { return _currentFileType; }
	QString			getCurrentFilePath()	const { return _currentFilePath; }
	QString			getDefaultOutFileName();

	void			showPreferences();

	FileOperation						fileoperation()				const	{ return _fileoperation;			}
	DataLibrary						*	datalibrary()				const	{ return _dataLibrary;				}
	CurrentDataFile					*	currentFile()				const	{ return _currentDataFile;			}
	RecentFiles						*	recentFiles()				const	{ return _recentFiles;				}
	AutoSaves						*	autoSaves()					const	{ return _autoSaves;				}
	Computer						*	computer()					const	{ return _computer;					}
	OSF								*	osf()						const	{ return _OSF;						}
	DatabaseFileMenu				*	database()					const	{ return _database;					}
	bool								visible()					const	{ return _visible;					}
	ActionButtons					*	actionButtons()				const	{ return _actionButtons;			}
	ResourceButtons					*	resourceButtons()			const	{ return _resourceButtons;			}
	ResourceButtonsVisible			*	resourceButtonsVisible()	const	{ return _resourceButtonsVisible;	}

	Q_INVOKABLE void exportResultsInteractive();
	Q_INVOKABLE void showAdvancedPreferences();
	
signals:
	void fileoperationChanged();
	void exportSelected(QString filename);
	void visibleChanged(bool visible);
	void dummyChangedNotifier();
	void showAbout();
	void showContact();
	void showCommunity();
	void modeChanged(FileEvent::FileMode mode);

public slots:
	void analysisAdded(Analysis *analysis);
	void workspaceModified();
	void setSyncFile(FileEvent *event);
	void dataAutoSynchronizationChanged(bool on) { setDataFileWatcher(on); }
	void startFileEvent()	override;
	void finalizeFileEvent()	override;
	void setFileoperation(const ActionButtons::FileOperation fo);
	void actionButtonClicked(const ActionButtons::FileOperation action);
	void setVisible(bool visible);
	void showFileOpenMenu();
	void resourceButtonClicked(const int buttonType);
	void showAboutRequest();
	void showContactRequest();
	void analysesExportResults();
	void refresh();
	void close();
	void enableButtonsForOpenedWorkspace(bool enableSaveButton = false);
	void buttonsForEmptyWorkspace();

private:
			bool checkSyncFileExists(const QString &path, bool waitForExistence = false);
			void clearSyncData();
			void setSyncRequest(const QString& path, bool waitForExistence = false);

	static	bool clearOSFFromRecentList(QString path);

private:
	OnlineDataManager			*	_odm						= nullptr;
	CurrentDataFile				*	_currentDataFile			= nullptr;
	RecentFiles					*	_recentFiles				= nullptr;
	AutoSaves					*	_autoSaves					= nullptr;
	Computer					*	_computer					= nullptr;
	OSF							*	_OSF						= nullptr;
	DatabaseFileMenu			*	_database					= nullptr;
	DataLibrary					*	_dataLibrary				= nullptr;
	ActionButtons				*	_actionButtons				= nullptr;
	ResourceButtons				*	_resourceButtons			= nullptr;
	ResourceButtonsVisible		*	_resourceButtonsVisible		= nullptr;

	
	FileEvent::FileMode				_mode						= FileEvent::FileOpen;
	QString							_currentFilePath;
	Utils::FileType					_currentFileType			= Utils::FileType::unknown;
	bool							_visible					= false;
	FileOperation					_fileoperation				= ActionButtons::None;
	MainWindow					*	_mainWindow					= nullptr;
};

#endif // FILEMENU_H
