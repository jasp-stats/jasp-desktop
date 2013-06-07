#include "ribbonbutton.h"

#include <QDebug>

RibbonButton::RibbonButton(QWidget *parent) :
    QPushButton(parent)
{

}

void RibbonButton::paintEvent(QPaintEvent *event)
{
    if (m_firstPaint)
    {
        m_firstPaint = false;

        QString style;

    #ifdef __APPLE__
        style = QString("QWidget { text-align: left ; height: 36px ; padding-right: 10px ; } QPushButton:hover { padding-left: 9px ; padding-top: -3px ; } QPushButton:!hover { border: none ; margin-top: 0px ; padding-left: 10px ; padding-top: -3px ; }");
    #else
		style = QString("QWidget { text-align: left ; height: 36px ; padding-right: 10px ; } QPushButton:hover { padding-left: 9px ; padding-top: -3px ; } QPushButton:!hover { border: none ; margin-top: 0px ; padding-left: 10px ; padding-top: -3px ; }");
	#endif

        setStyleSheet(style);
    }

    QPushButton::paintEvent(event);
}
