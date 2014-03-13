#ifndef STEALTHBUTTON_H
#define STEALTHBUTTON_H

#include <QPushButton>

#include "common.h"

class StealthButton : public QPushButton
{
	Q_OBJECT
public:
	explicit StealthButton(QWidget *parent = 0);

protected:
	virtual void paintEvent(QPaintEvent *event) OVERRIDE;

private:
	bool _firstPaint;

};

#endif // STEALTHBUTTON_H
