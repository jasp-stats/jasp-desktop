#ifndef BUTTON_H
#define BUTTON_H

#include <QPushButton>
#include "common.h"

class Button : public QPushButton
{
	Q_OBJECT

public:
	explicit Button(QWidget *parent = 0);

protected:
	bool event(QEvent *e) OVERRIDE;

};

#endif // BUTTON_H
