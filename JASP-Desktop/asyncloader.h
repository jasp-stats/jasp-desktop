//
// Copyright (C) 2013-2015 University of Amsterdam
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
#include <QThread>
#include <QMutex>
#include <QTimer>

#include "dataset.h"
#include "datasetloader.h"
#include "datasetpackage.h"
#include "fileevent.h"

class AsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit AsyncLoader(QObject *parent = 0);

	void io(FileEvent *event, DataSetPackage *package);
	void free(DataSet *dataSet);

signals:
	void beginLoad(FileEvent*, DataSetPackage*);
	void beginSave(FileEvent*, DataSetPackage*);
	void progress(const QString &status, int progress);

private slots:
	void loadTask(FileEvent *event, DataSetPackage *package);
	void saveTask(FileEvent *event, DataSetPackage *package);

private:

	void sleep(int ms);
	void progressHandler(std::string status, int progress);
	QThread _thread;
	DataSetLoader _loader;
	
};

#endif // ASYNCLOADER_H
