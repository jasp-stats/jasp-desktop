#include "toolbutton.h"

#include <QDebug>

ToolButton::ToolButton(QWidget *parent) :
    QPushButton(parent)
{
    m_firstPaint = true;
}

void ToolButton::paintEvent(QPaintEvent *event)
{
    if (m_firstPaint)
    {
        m_firstPaint = false;

        QString style;

    #ifdef __APPLE__
        style = QString("QWidget { height: 36px ; padding-right: 10px ; } QPushButton:hover { padding-left: 9px ; padding-top: -3px ; } QPushButton:!hover { border: none ; margin-top: 0px ; padding-left: 9px ; padding-top: -3px ; }");
    #else
        style = QString("QWidget { height: 28px ; } QPushButton:!hover { border: none ; }");
    #endif

        setStyleSheet(style);

    }

    QPushButton::paintEvent(event);
}

