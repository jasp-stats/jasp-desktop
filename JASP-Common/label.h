#ifndef LABEL_H
#define LABEL_H

#include <string>

class Label
{
public:
	Label(const std::string &label, int value);
	Label(int value);
	Label();

	std::string text() const;
	bool hasIntValue() const;
	int value() const;
	Label& operator=(const Label &label);

private:

	bool _hasIntValue;
	int _intValue;
	char _stringValue[128];
	int  _stringLength;
};

#endif // LABEL_H
