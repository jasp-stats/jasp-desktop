//
// Copyright (C) 2017 University of Amsterdam
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
//

#ifndef OPENWIDGET_H
#define OPENWIDGET_H

#include <QWidget>

#include <QWebView>
#include <QFileSystemWatcher>
#include <QSettings>

#include "verticaltabwidget.h"
#include "fsbmrecent.h"
#include "fsbmcurrent.h"
#include "fsbmexamples.h"
#include "fsbrowser.h"

#include "backstagecomputer.h"
#include "backstageosf.h"

#include "fileevent.h"

class OpenSaveWidget : public QWidget
{
	Q_OBJECT
public:
	enum FileLocation {Recent = 0, Current, Computer, OSF, Examples};

	explicit OpenSaveWidget(QWidget *parent = 0);

	VerticalTabWidget *tabWidget();
	void setSaveMode(FileEvent::FileMode mode);
	void setOnlineDataManager(OnlineDataManager *odm);
	bool changeTabIfCurrentFileEmpty();

	FileEvent* open();
	FileEvent* open(const QString &path);
	FileEvent* save();
	void sync();
	FileEvent *close();

	void setCurrentDataFile(const QString &path);
	void setDataFileWatcher(bool watch);

	Utils::FileType getCurrentFileType();

public slots:
	void dataSetIOCompleted(FileEvent *event);

signals:
	void dataSetIORequest(FileEvent *event);

private slots:
	void dataSetIORequestHandler(FileEvent *event);
	void dataSetOpenRequestHandler(QString path);
	void dataSetOpenExampleRequestHandler(QString path);
	void dataSetOpenCurrentRequestHandler(QString path);
	void dataFileModifiedHandler(QString path);
	void clearOnlineDataFromRecentList(int provider);
	void tabWidgetChanged(int id);
	void tabWidgetChanging(int index, bool &cancel);

private:
	bool checkSyncFileExists(const QString &path);
	void clearSyncData();

	static bool clearOSFFromRecentList(QString path);
	OnlineDataManager *_odm = NULL;

	QString _currentFilePath;
	Utils::FileType _currentFileType;

	FileEvent::FileMode _mode;

	VerticalTabWidget *_tabWidget;

	FSBMRecent   *_fsmRecent;
	FSBMCurrent   *_fsmCurrent;
	FSBMExamples *_fsmExamples;

	FSBrowser *_bsRecent;
	FSBrowser *_bsCurrent;
	BackstageComputer *_bsComputer;
	BackstageOSF *_bsOSF;
	FSBrowser *_bsExamples;

	QFileSystemWatcher _watcher;
	QSettings _settings;
};

#endif // OPENWIDGET_H
