#include "boundgroupbox.h"

#include <QTimer>
#include <QRadioButton>
#include <QDebug>

#include <boost/foreach.hpp>

BoundGroupBox::BoundGroupBox(QWidget *parent) :
	QWidget(parent)
{
	_option = NULL;
	_timer = new QTimer(this);
	_buttonGroup = new QButtonGroup(this);

	connect(_buttonGroup, SIGNAL(buttonClicked(QAbstractButton*)), this, SLOT(itemSelected(QAbstractButton*)));
}

void BoundGroupBox::bindTo(Option *option)
{
	_option = dynamic_cast<OptionList *>(option);
}

void BoundGroupBox::childEvent(QChildEvent *child)
{
	if (child->added())
		_timer->singleShot(0, this, SLOT(updateGroup()));
}

void BoundGroupBox::updateGroup()
{
	BOOST_FOREACH(QAbstractButton *button, _buttonGroup->buttons())
		_buttonGroup->removeButton(button);

	BOOST_FOREACH(QObject *child, this->children())
	{
		QRadioButton *radio = qobject_cast<QRadioButton *>(child);
		if (radio != NULL)
		{
			_buttonGroup->addButton(radio);
			if (_option != NULL && QString(_option->value().c_str()) == radio->objectName())
				radio->setChecked(true);
		}
	}
}

void BoundGroupBox::itemSelected(QAbstractButton *button)
{
	QString name = button->objectName();
	QByteArray buffer = name.toUtf8();
	std::string value(buffer.constData(), buffer.length());

	if (_option != NULL)
		_option->setValue(value);
}

