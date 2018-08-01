//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

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
