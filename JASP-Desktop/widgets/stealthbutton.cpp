//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "stealthbutton.h"

StealthButton::StealthButton(QWidget *parent) :
	QPushButton(parent)
{
	_firstPaint = true;
}

void StealthButton::paintEvent(QPaintEvent *event)
{
	if (_firstPaint)
	{
		_firstPaint = false;

		QString style;

#ifdef __APPLE__
		style = QString("QWidget { text-align: left ; height: 36px ; padding-right: 10px ; } QPushButton:hover { padding-left: 9px ; padding-top: -3px ; } QPushButton:!hover { border: none ; margin-top: 0px ; padding-left: 10px ; padding-top: -3px ; } QPushButton::menu-indicator { image: none ; }");
		//style = QString("QWidget { text-align: left ; height: 36px ; padding-right: 10px ; } /*QPushButton:hover { padding-left: 9px ; padding-top: -3px ; }*/ QPushButton:!hover { border: none ; margin-top: 0px ; padding-left: 10px ; padding-top: -3px ; }");
		//style = QString("QWidget { text-align: left ; height: 32px ; padding-left: 10px ; padding-right: 10px ; padding-top: -3px ; } QPushButton:!hover { border: none ; padding-left: 1px ; padding-top: -3px ; }");
#else
        style = QString("QWidget { text-align: left ; height: 36px ; padding-right: 10px ; } QPushButton:hover { padding-left: 9px ; padding-top: -3px ; } QPushButton:!hover { border: none ; margin-top: 0px ; padding-left: 10px ; padding-top: -3px ; font-size: 12px ; }");
#endif

		setStyleSheet(style);
	}

	QPushButton::paintEvent(event);
}

