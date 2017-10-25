#ifndef MODULE_H
#define MODULE_H

#include <QString>
#include <map>

class Module
{
public:
	static std::map<QString, Module> AllModules;
	static const Module &getModule(QString name);
	static bool isModuleName(QString name);

	Module(QString name, int ribbonIndex = 0, bool released = true);
	Module(QString name, QString displayName, int ribbonIndex, bool released = true);

	QString name() const { return _name; }
	QString displayName() const { return _displayName; }
	int ribbonIndex() const { return _ribbonIndex; }
	bool released() const { return _released; }

private:
	QString _name;
	QString _displayName;
	int _ribbonIndex;
	bool _released;
};

#endif // MODULE_H
