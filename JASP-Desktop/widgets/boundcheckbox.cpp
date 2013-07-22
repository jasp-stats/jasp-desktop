#include "boundcheckbox.h"

#include <QDebug>

BoundCheckBox::BoundCheckBox(QWidget *parent) :
    QCheckBox(parent)
{
	_boundTo = NULL;
}

void BoundCheckBox::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionBoolean *>(option);

	if (_boundTo == NULL)
		qDebug() << "could not bind to OptionBoolean in boundcheckbox.cpp";
}

void BoundCheckBox::nextCheckState()
{
	QCheckBox::nextCheckState();

	if (_boundTo != NULL)
		_boundTo->setValue(isChecked());
}
