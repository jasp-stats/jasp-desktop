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

	_mouseOutSS = "QToolButton {"
			"	border: 1px solid transparent ;"
			"	border-radius: 5px ;"
			"}"
			"QToolButton[popupMode='2'] {"
			"	padding-right: 4px ;"
			"}"
			""
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

