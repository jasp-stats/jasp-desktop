#ifndef EXPANDERBUTTON_H
#define EXPANDERBUTTON_H

#include <QPushButton>

#include "common.h"

class ExpanderButton : public QPushButton
{
	Q_OBJECT
public:
	explicit ExpanderButton(QWidget *parent = 0);
	virtual QSize sizeHint() const OVERRIDE;
	virtual QSize minimumSizeHint() const OVERRIDE;

protected:
	virtual void nextCheckState() OVERRIDE;

private:
    bool _expanded;
    QIcon _expandedIcon;
    QIcon _contractedIcon;
	
};

#endif // EXPANDERBUTTON_H
