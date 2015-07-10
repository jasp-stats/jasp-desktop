
#include "button.h"

#include <QEvent>

Button::Button(QWidget *parent) : QPushButton(parent)
{
}

bool Button::event(QEvent *e)
{
	if (e->type() == QEvent::Wheel && this->isEnabled() == false)
		return false;

	return QPushButton::event(e);
}
