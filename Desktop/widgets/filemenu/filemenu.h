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
#include "widgets/filemenu/computer.h"
#include "widgets/filemenu/osf.h"
#include "widgets/filemenu/databasefilemenu.h"
#include "widgets/filemenu/datalibrary.h"

#include "data/fileevent.h"
#include "filemenulistitem.h"
#include "actionbuttons.h"
#include "resourcebuttonsvisible.h"

class MainWindow;
class DataSetPackage;

class FileMenu : public QObject
{
	friend FileMenuObject;
	
	typedef ActionButtons::FileOperation FileOperation;
	Q_OBJECT

	Q_PROPERTY(FileOperation				fileoperation			READ fileoperation				WRITE setFileoperation				NOTIFY fileoperationChanged		)
	Q_PROPERTY(DataLibrary				*	datalibrary				READ datalibrary													NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(CurrentDataFile			*	currentFile				READ currentFile													NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(RecentFiles				*	recentFiles				READ recentFiles													NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(Computer					*	computer				READ computer														NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(OSF						*	osf						READ osf															NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(DatabaseFileMenu			*	database				READ database														NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(bool							visible					READ visible					WRITE setVisible					NOTIFY visibleChanged			)
	Q_PROPERTY(ActionButtons			*	actionButtons			READ actionButtons													NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(ResourceButtons			*	resourceButtons			READ resourceButtons												NOTIFY dummyChangedNotifier		)
	Q_PROPERTY(ResourceButtonsVisible	*	resourceButtonsVisible	READ resourceButtonsVisible											NOTIFY dummyChangedNotifier		)

public:

	enum FileLocation { Recent = 0, Current, ThisComputer, Osf, Examples, CountLocations };

	Q_ENUM(FileMenuListItemType)

	explicit FileMenu(QObject *parent = nullptr);
	virtual ~FileMenu() {}
	

	void		setResourceButtonsVisibleFor(FileOperation fo);

	void		setOnlineDataManager(OnlineDataManager *odm);
	FileEvent *	open(const QString &filepath);
	FileEvent *	save();
	void		sync();

	void			setCurrentDataFile(const QString		& path);
	void			setDataFileWatcher(bool watch);
	
	void			setSaveMode(FileEvent::FileMode mode);
	Utils::FileType getCurrentFileType()	const { return _currentFileType; }
	QString			getCurrentFilePath()	const { return _currentFilePath; }
	QString			getDefaultOutFileName();
	bool			isCurrentFileReadOnly() const { return _currentFileReadOnly; }

	void			showPreferences();
	void			syncDataFile(const QString& path, bool waitForExistence = false);


	FileOperation						fileoperation()				const	{ return _fileoperation;			}
	DataLibrary						*	datalibrary()				const	{ return _dataLibrary;				}
	CurrentDataFile					*	currentFile()				const	{ return _currentDataFile;			}
	RecentFiles						*	recentFiles()				const	{ return _recentFiles;				}
	Computer						*	computer()					const	{ return _computer;					}
	OSF								*	osf()						const	{ return _OSF;						}
	DatabaseFileMenu						*	database()					const	{ return _database;					}
	bool								visible()					const	{ return _visible;					}
	ActionButtons					*	actionButtons()				const	{ return _actionButtons;			}
	ResourceButtons					*	resourceButtons()			const	{ return _resourceButtons;			}
	ResourceButtonsVisible			*	resourceButtonsVisible()	const	{ return _resourceButtonsVisible;	}

	Q_INVOKABLE void exportResultsInteractive();

signals:
	void fileoperationChanged();
	void dataSetIORequest(FileEvent *event);
	void exportSelected(QString filename);
	void visibleChanged(bool visible);
	void dummyChangedNotifier();
	void showAbout();

public slots:
	void analysisAdded(Analysis *analysis);
	void setSyncFile(FileEvent *event);
	void dataAutoSynchronizationChanged(bool on) { setDataFileWatcher(on); }
	void dataSetIOCompleted(FileEvent *event);
	void dataFileModifiedHandler(QString path);
	void setFileoperation(const ActionButtons::FileOperation fo);
	void actionButtonClicked(const ActionButtons::FileOperation action);
	void setVisible(bool visible);
	void showFileOpenMenu();
	void resourceButtonClicked(const int buttonType);
	void showAboutRequest();
	void dataColumnAdded(QString columnName);
    void analysesExportResults();
	void refresh();
	void close();



private slots:
	void dataSetIORequestHandler(FileEvent *event);

private:
			bool checkSyncFileExists(const QString &path, bool waitForExistence = false);
			void clearSyncData();
			void setSyncRequest(const QString& path, bool waitForExistence = false);

	static	bool clearOSFFromRecentList(QString path);

private:
	OnlineDataManager			*	_odm						= nullptr;
	CurrentDataFile				*	_currentDataFile			= nullptr;
	RecentFiles					*	_recentFiles				= nullptr;
	Computer					*	_computer					= nullptr;
	OSF							*	_OSF						= nullptr;
	DatabaseFileMenu					*	_database					= nullptr;
	DataLibrary					*	_dataLibrary				= nullptr;
	ActionButtons				*	_actionButtons				= nullptr;
	ResourceButtons				*	_resourceButtons			= nullptr;
	ResourceButtonsVisible		*	_resourceButtonsVisible		= nullptr;

	QFileSystemWatcher				_watcher;
	FileEvent::FileMode				_mode						= FileEvent::FileOpen;
	QString							_currentFilePath;
	Utils::FileType					_currentFileType			= Utils::FileType::unknown;
	bool							_currentFileReadOnly		= false,
									_visible					= false;
	FileOperation					_fileoperation				= ActionButtons::None;
	MainWindow*						_mainWindow					= nullptr;
};

#endif // FILEMENU_H
