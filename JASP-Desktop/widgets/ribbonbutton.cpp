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

#include "ribbonbutton.h"

#include <QDebug>
#include <QMouseEvent>
#include <QMenu>

RibbonButton::RibbonButton(QWidget *parent) :
	QToolButton(parent)
{
	/* all this mouse over stuff is to work around
	 * a bug where if the mouse leaves the button
	 * while the menu is visible, it doesn't register
	 * the mouse out, and keeps the mouse-over background */

	_mouseOver = false;
	_connectedToMenu = false;
	_dataSetNeeded = true;

	_mouseOutSS = "QToolButton {"
			"	border: 1px solid transparent ;"
			"	border-radius: 5px ;"
			"	padding-top: 4px ;"
			"}"
			"QToolButton[popupMode='2'] {"
			"	padding-right: 4px ;"
			"}"
			""
			"QToolButton[button-type='summarize'] {"
			"	padding-right: 0px;"
			"}"
			"QToolButton::menu-indicator {"
			"	image: url(:/icons/toolbutton-menu-indicator.svg) ;"
			"	width: 8px ;"
			"	bottom: +28px ;"
			"	left : -7px ;"
			"}"
			""
			""
			"QToolButton::pressed {"
			"	background-color: rgb(212, 210, 211) ;"
			"	border: 1px solid #B0B0B0 ;"
			"}\n";

	_mouseOverSS =
			"QToolButton::hover"
			"{"
			"	background-color: rgb(227, 225, 226) ;"
			"	border : 1px solid rgb(207, 205, 206) ;"
			"}"
			"" \
			+ _mouseOutSS;

	setStyleSheet(_mouseOutSS);

}

void RibbonButton::setDataSetNotNeeded()
{
	_dataSetNeeded = false;
}

bool RibbonButton::isDataSetNeeded() const
{
	return _dataSetNeeded;
}

void RibbonButton::notifyMouseOut()
{
	if (_mouseOver)
	{
		_mouseOver = false;
		setStyleSheet(_mouseOutSS);
	}
}

void RibbonButton::notifyMouseOver()
{
	if ( ! _mouseOver)
	{
		_mouseOver = true;
		setStyleSheet(_mouseOverSS);
	}
}

void RibbonButton::enterEvent(QEvent *event)
{
	notifyMouseOver();

	QToolButton::enterEvent(event);
}

void RibbonButton::mousePressEvent(QMouseEvent *event)
{
	if (_connectedToMenu == false)
	{
		QMenu *m = menu();
		if (m != NULL)
			connect(m, SIGNAL(aboutToHide()), this, SLOT(notifyMouseOut()));

		_connectedToMenu = true;
	}

	QToolButton::mousePressEvent(event);
}

