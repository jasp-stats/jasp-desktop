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
		style = QString("QWidget { text-align: left ; height: 36px ; padding-right: 10px ; } QPushButton:hover { padding-left: 9px ; padding-top: -3px ; } QPushButton:!hover { border: none ; margin-top: 0px ; padding-left: 10px ; padding-top: -3px ; }");
#endif

		setStyleSheet(style);
	}

	QPushButton::paintEvent(event);
}

