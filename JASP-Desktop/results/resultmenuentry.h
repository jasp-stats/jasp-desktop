//
// Copyright (C) 2013-2018 University of Amsterdam
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


#ifndef RESULTMENUENTRY_H
#define RESULTMENUENTRY_H

#include <QString>
#include <map>

class ResultMenuEntry
{
public:
	static std::map<QString, ResultMenuEntry> AllResultEntries;

	ResultMenuEntry(QString displayText,
					QString name,
					QString menuImageSource,
					QString jsFunction);

	QString	displayText()						const	{	return _displayText;			}
	QString	name()								const	{	return _name;					}
	QString	menuImageSource()					const	{	return _menuImageSource;		}
	QString	jsFunction()						const	{	return _jsFunction;				}

	void	setDisplayText(QString displayText)			{	_displayText = displayText;		}
	void	setImageSource(QString imageSource)			{	_menuImageSource = imageSource;	}
	void	setJSFunction(QString jsFunction)			{	_jsFunction = jsFunction;		}

private:
	QString	_displayText,
			_name,
			_menuImageSource,
			_jsFunction;
};

#endif // RESULTMENUENTRY_H
