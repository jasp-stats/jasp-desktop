#ifndef FSENTRY_H
#define FSENTRY_H

#include <QString>

class FSEntry
{
public:
	enum EntryType { JASP = 0, CSV = 1, SPSS = 2, Folder = 3, Other = 4, NoOfTypes = 5 };

	FSEntry() { entryType = Other; }

	QString name;
	QString path;
	QString description;
	EntryType entryType;

};

#endif // FSENTRY_H
