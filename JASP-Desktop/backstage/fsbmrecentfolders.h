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

#ifndef FSBROWSERMODELRECENTFOLDERS_H
#define FSBROWSERMODELRECENTFOLDERS_H

#include "fsbmodel.h"
#include "common.h"

#include <QSettings>

class FSBMRecentFolders : public FSBModel
{
public:
	explicit FSBMRecentFolders(QObject *parent = NULL);

	void refresh() OVERRIDE;

	QString mostRecent() const;

public slots:
	void addRecent(QString path);

private:

	QStringList readRecents();
	void setRecents(const QStringList &recents);
	void setAndSaveRecents(const QStringList &recents);
	void saveRecents();

	QStringList _recents;
	QSettings _settings;
};

#endif // FSBROWSERMODELRECENTFOLDERS_H
