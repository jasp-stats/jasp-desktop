#ifndef IMPORTCOLUMN_H
#define IMPORTCOLUMN_H

#include <string>
#include <map>
#include <vector>

using namespace std;

class ImportColumn
{
public:
	ImportColumn(string name, string longName = "");
	virtual ~ImportColumn();
	virtual int size() const = 0;
	virtual bool isValueEqual(int row, string value) = 0;

	string getName() const;
	string getLongName() const;

private:
	string _name;
	string _longName;
};

#endif // IMPORTCOLUMN_H
