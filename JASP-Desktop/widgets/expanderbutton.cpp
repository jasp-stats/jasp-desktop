#include "expanderbutton.h"

#include <QDialog>
#include <QIcon>
#include <QImage>
#include <QDebug>

ExpanderButton::ExpanderButton(QWidget *parent) :
	QPushButton(parent)
{
    _expanded = false;
    _expandedIcon = QIcon(QString(":/images/expander-arrow-down.png"));
    _contractedIcon = QIcon(QString(":/images/expander-arrow-up.png"));

	setCheckable(true);
	setIcon(_contractedIcon);

#ifdef __APPLE__
	setStyleSheet(QString("QPushButton { text-align: left ; }"));
#else
	setStyleSheet(QString("QPushButton { text-align: left ; padding: 3px 9px ; }"));
#endif

}

void ExpanderButton::nextCheckState()
{
	_expanded = !_expanded;
	setIcon(_expanded ? _expandedIcon : _contractedIcon);

	QObjectList siblings = this->parentWidget()->children();

	for (QObjectList::Iterator itr = siblings.begin(); itr != siblings.end(); itr++) {
		QWidget* w = dynamic_cast<QWidget*>(*itr);
        if (w != NULL && w != this)
			w->setVisible(_expanded);
	}
}
