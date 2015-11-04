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

	setFocusPolicy(Qt::NoFocus);

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

QSize ExpanderButton::sizeHint() const {
	 QSize sizeHint = QPushButton::sizeHint();

	 int width = sizeHint.width();
	QObjectList siblings = this->parentWidget()->children();
	for (QObjectList::Iterator itr = siblings.begin(); itr != siblings.end(); itr++) {
		QWidget* w = dynamic_cast<QWidget*>(*itr);
		if (w != NULL && w != this && w->sizeHint().width() > width)
			width = w->sizeHint().width();
	}

	sizeHint.setWidth(width);

	return sizeHint;
}

QSize ExpanderButton::minimumSizeHint() const {
	 QSize sizeHint = QPushButton::minimumSizeHint();

	 int width = sizeHint.width();
	QObjectList siblings = this->parentWidget()->children();
	for (QObjectList::Iterator itr = siblings.begin(); itr != siblings.end(); itr++) {
		QWidget* w = dynamic_cast<QWidget*>(*itr);
		if (w != NULL && w != this && w->minimumSizeHint().width() > width)
			width = w->minimumSizeHint().width();
	}

	sizeHint.setWidth(width);

	return sizeHint;
}
