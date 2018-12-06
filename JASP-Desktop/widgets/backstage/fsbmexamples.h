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

#ifndef FSBMEXAMPLES_H
#define FSBMEXAMPLES_H

#include "fsbmodel.h"
#include "common.h"

class ExtendedFSEntry: public FSEntry		
{
public:
	QString associated_datafile = "";	
};

class FSBMExamples : public FSBModel
{
	Q_OBJECT

public:
	FSBMExamples(QObject *parent = NULL, QString root = "");
	~FSBMExamples();
	void refresh() OVERRIDE;
	typedef QList<ExtendedFSEntry> FileSystemExtendedEntryList;
	static const QString rootelementname; //Root element in index.json
	const FileSystemExtendedEntryList &entries() const;
	
private:
	void loadRootElements();
	void loadFilesAndFolders(const QString &path);
	QJsonDocument *getJsonDocument();
	QJsonDocument *_doc;
	bool isFolder(const QString &kind);	
	QString _dataLibraryRootPath;
	
	static ExtendedFSEntry createEntry(const QString &path, const QString &name, const QString &description, FSEntry::EntryType type = FSEntry::Other, const QString &associated_datafile = "");	
	FileSystemExtendedEntryList _entries;

protected:
	
	
};

#endif // FSBMEXAMPLES_H
