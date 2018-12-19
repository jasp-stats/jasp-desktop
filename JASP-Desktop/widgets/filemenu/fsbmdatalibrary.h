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

#ifndef FSBMDATALIBRARY_H
#define FSBMDATALIBRARY_H

#include "fsbmodel.h"
#include "common.h"

class FSBMDataLibrary : public FSBModel
{
	Q_OBJECT

public:
	FSBMDataLibrary(QObject *parent = NULL, QString root = "");
	~FSBMDataLibrary();
	void refresh() OVERRIDE;
	static const QString rootelementname; //Root element in index.json
	
private:
	void			loadRootElements();
	void			loadFilesAndFolders(const QString &path);
	QJsonDocument*	getJsonDocument();
	bool			isFolder(const QString &kind);

	QJsonDocument	*_doc;
	QString			_dataLibraryRootPath;
	
	
};

#endif // FSBMDATALIBRARY_H
