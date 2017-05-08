//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef BACKSTAGEWIDGET_H
#define BACKSTAGEWIDGET_H

#include <QWidget>

#include <QStackedWidget>

#include "backstage/verticaltabbar.h"
#include "backstage/opensavewidget.h"
#include "fileevent.h"

#include "activitylog.h"
#include "analysis.h"

class BackStageWidget : public QWidget
{
	Q_OBJECT
public:
	enum FileOperation {Open = 0, Save, SaveAs, ExportResults, ExportData, SyncData, Close};
	explicit BackStageWidget(QWidget *parent = NULL);
	void setOnlineDataManager(OnlineDataManager *odm);

	void setLog(ActivityLog *log);
	FileEvent *open();
	FileEvent *open(const QString &filepath);
	FileEvent *save();
	void sync();
	FileEvent *close();

signals:
	void dataSetIORequest(FileEvent *event);
	void exportSelected(QString filename);

public slots:
	void analysisAdded(Analysis *analysis);
	void setSyncFile(FileEvent *event);
	void dataAutoSynchronizationChanged(bool on);

private slots:
	void tabPageChanging(int index, bool &cancel);

	void dataSetIORequestHandler(FileEvent *event);
	void dataSetIORequestCompleted(FileEvent *event);

private:
	VerticalTabBar *_tabBar;
	QStackedWidget *_tabPages;

	OpenSaveWidget *_openAndSaveWidget;
};

#endif // BACKSTAGEWIDGET_H
