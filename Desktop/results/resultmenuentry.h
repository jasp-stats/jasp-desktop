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

#include <map>
#include <QString>
#include <QStringList>


class ResultMenuEntry
{
public:
	ResultMenuEntry(QString displayText,
					QString name,
					QString menuImageSource,
					QString jsFunction);
	ResultMenuEntry();

	QString	displayText()						const	{	return _displayText;			}
	QString	name()								const	{	return _name;					}
	QString	menuImageSource()					const;
	QString	jsFunction()						const	{	return _jsFunction;				}
	bool	isSeparator()						const	{	return _isSeparator;			}
	bool	isEnabled()							const	{	return _isEnabled;				}

	void	setDisplayText(QString displayText)			{	_displayText = displayText;		}
	void	setImageSource(QString imageSource)			{	_menuImageSource = imageSource;	}
	void	setJSFunction(QString jsFunction)			{	_jsFunction = jsFunction;		}
	void	setEnabled(bool enabled)					{	_isEnabled = enabled;			}

private:
	QString	_displayText,
			_name,
			_menuImageSource,
			_jsFunction;
	bool	_isSeparator = false,
			_isEnabled = true;
};

#endif // RESULTMENUENTRY_H
