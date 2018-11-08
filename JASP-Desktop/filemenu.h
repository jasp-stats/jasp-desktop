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

#include "mainwindow.h"

#include <QObject>
#include <QQuickWidget>
#include <QFileSystemWatcher>
#include <QVector>
#include <QQmlPropertyMap>

#include "widgets/backstage/backstagerecentfiles.h"
#include "widgets/backstage/backstagecurrentfile.h"
#include "widgets/backstage/backstagecomputer.h"
#include "widgets/backstage/backstageosf.h"
#include "widgets/backstage/backstagedatalibrary.h"

class MainWindow;

class FileMenu : public QObject
{
	Q_OBJECT
	
	Q_PROPERTY(	bool enable_currentfile_button	READ enable_currentfile_button	WRITE set_enable_currentfile_button	NOTIFY enable_currentfile_button_changed)
	Q_PROPERTY(	FileOperation fileoperation		READ fileoperation				WRITE setFileoperation				NOTIFY fileoperationChanged)
	Q_PROPERTY(	QVector<bool> buttonsenabled	READ buttonsenabled				WRITE setButtonsenabled				NOTIFY buttonsenabledChanged)

public:
	
	enum FileOperation {Open = 0, Save, SaveAs, ExportResults, ExportData, SyncData, Close, NoFileActions};
	Q_ENUM(FileOperation)
	enum FileLocation {Recent = 0, Current, Computer, OSF, Examples};
	
	explicit FileMenu(QWidget *parent = nullptr);
	
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
		
signals:
	
	void enable_currentfile_button_changed();
	void fileoperationChanged();
	void buttonsenabledChanged();
	
	//Backstage
	void dataSetIORequest(FileEvent *event);
	void exportSelected(QString filename);

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
	
	MainWindow *_mainWindow;
	QQuickWidget *_qquickfilemenu;
	
	OnlineDataManager *_odm = nullptr;
	FileEvent::FileMode _mode;
	
	QString _currentFilePath;
	Utils::FileType _currentFileType;
	bool _currentFileReadOnly;
	int _selectedActionIndex;
	int _selectedResourceIndex;
	
	BackstageCurrentFile *_bsCurrentFile;
	BackstageRecentFiles *_bsRecentFiles;
	BackstageComputer *_bsComputer;
	BackstageOSF *_bsOSF;
	BackstageDataLibrary *_bsDataLibrary;
	
	QFileSystemWatcher _watcher;	
			
	bool _enable_currentfile_button;

	FileOperation _fileoperation = Close;
	QVector<bool> _buttonsenabled;
	QQmlPropertyMap fileMenuProperties;
};

#endif // FILEMENU_H
