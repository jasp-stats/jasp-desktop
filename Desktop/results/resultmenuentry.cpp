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

#include "resultmenuentry.h"
#include "jasptheme.h"

//the window.blabla() functions are defined main.js and the hasBlabla are defined jaspwidgets.js

ResultMenuEntry::ResultMenuEntry(QString displayText, QString name, QString menuImageSource, QString jsFunction)
	: _displayText(displayText)
	, _name(name)
	, _menuImageSource(menuImageSource)
	, _jsFunction(jsFunction)
	, _isSeparator(false)
	, _isEnabled(true)
{
}

ResultMenuEntry::ResultMenuEntry()
{
	_isSeparator = true;
}

QString	ResultMenuEntry::menuImageSource() const
{

	return _menuImageSource == "" ? "" : JaspTheme::currentIconPath() + _menuImageSource;
}
