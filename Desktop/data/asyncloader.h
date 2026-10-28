//
// Copyright (C) 2013-2026 University of Amsterdam
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

#define BOOST_BIND_GLOBAL_PLACEHOLDERS
#ifndef ASYNCLOADER_H
#define ASYNCLOADER_H

#include <QObject>
#include <QMutex>
#include <QTimer>

#include "datasetloader.h"
#include "datasetpackage.h"
#include "data/fileevent.h"
#include "data/exporters/exporter.h"

#include "osf/onlinedatamanager.h"
#include "timers.h"

struct LoaderException : public std::runtime_error
{
	LoaderException(const std::string & _problemDescription, bool _cancelled = false);

	bool cancelled = false;
};

///
/// Used to run importers and exporters in a different thread from the main event loop.
/// This way we can keep the interface responsive but it is important to make sure the right kind of qt connections are used.
/// And no direct calls to the other threads...
class AsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit AsyncLoader(QObject *parent = 0);

	void io(FileEvent *event);
	void setOnlineDataManager(OnlineDataManager *odm);

signals:
	void beginLoad(FileEvent*);
	void beginSave(FileEvent*);
	void progress(QString status, int progress);
	void beginFileUpload(QString nodePath, QString sourcePath);
	bool checkDoSync();
	void syncCompleted(int dataSetId, bool success);
	///Emitted after a (non-sync) load added a dataset to the workspace. The workspace table model
	///was mutated on the loader thread, so the GUI thread connects to this to refresh it (and any
	///views bound to it, e.g. the dataset tabbuttons) from the correct thread.
	void dataSetsChanged();

public slots:
	///Carries the DataSet (not just its id) so the loader never has to reach into the GUI-owned
	///workspace map from its worker thread. The id is kept only for the syncCompleted completion
	///routing, which runs back on the GUI thread.
	void onSyncRequired(DataSet * dataSet, const QString & locator, const QString & extension, const QString & databaseJson);

private slots:
	void loadTask(FileEvent *event);
	void saveTask(FileEvent *event);
	void loadPackage(QString id);
	void uploadFileFinished(QString id);
	//void errorFlagged(QString msg, QString id);

protected:
	void progressHandler(int progress);

private:
	QString fileChecksum(const QString &fileName, QCryptographicHash::Algorithm hashAlgorithm);

	DataSetLoader			_loader;
	FileEvent			*	_currentEvent	= nullptr;
	OnlineDataManager	*	_odm			= nullptr;
};

#endif // ASYNCLOADER_H
