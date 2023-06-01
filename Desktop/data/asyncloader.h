//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef ASYNCLOADER_H
#define ASYNCLOADER_H

#include <QObject>
#include <QMutex>
#include <QTimer>

#include "dataset.h"
#include "datasetloader.h"
#include "datasetpackage.h"
#include "data/fileevent.h"

#include "osf/onlinedatamanager.h"

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
	void free(DataSet *dataSet);
	void setOnlineDataManager(OnlineDataManager *odm);

signals:
	void beginLoad(FileEvent*);
	void beginSave(FileEvent*);
	void progress(QString status, int progress);
	void beginFileUpload(QString nodePath, QString sourcePath);
	bool checkDoSync();

private slots:
	void loadTask(FileEvent *event);
	void saveTask(FileEvent *event);
	void loadPackage(QString id);
	void uploadFileFinished(QString id);
	//void errorFlagged(QString msg, QString id);

private:

	QString fileChecksum(const QString &fileName, QCryptographicHash::Algorithm hashAlgorithm);

	void progressHandler(int progress);



	DataSetLoader			_loader;
	FileEvent			*	_currentEvent	= nullptr;
	OnlineDataManager	*	_odm			= nullptr;
};

#endif // ASYNCLOADER_H
