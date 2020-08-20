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
//

#ifndef COMPUTER_H
#define COMPUTER_H

#include "filemenuobject.h"
#include "computerlistmodel.h"

class Computer : public FileMenuObject
{
	Q_OBJECT
	Q_PROPERTY(ComputerListModel * listModel READ listModel WRITE setListModel NOTIFY listModelChanged)
	
public:
	explicit Computer(QObject *parent = nullptr);

	void setFileName(const QString &filename);
	void clearFileName();
	void addRecentFolder(const QString &path);

	ComputerListModel * listModel() const { return _computerListModel; }
    void analysesExportResults();

protected:
	//bool eventFilter(QObject *object, QEvent *event) OVERRIDE;

public slots:
	void		browsePath(QString path);
	void		browseMostRecent();
	FileEvent *	browseOpen(const QString &path = "");
	FileEvent *	browseSave(const QString &path = "", FileEvent::FileMode mode = FileEvent::FileSave);
	void		setListModel(ComputerListModel * listModel);

signals:
	void listModelChanged(ComputerListModel * listModel);
	void browseOpenSignal(const QString & path = "");
	void browseSaveSignal(const QString & path = "", FileEvent::FileMode mode = FileEvent::FileSave);

private:
	// these two variables are a hack!
	bool	_hasFileName;
	QString _fileName;	
	
	ComputerListModel *_computerListModel = nullptr;
};

#endif // COMPUTER_H
