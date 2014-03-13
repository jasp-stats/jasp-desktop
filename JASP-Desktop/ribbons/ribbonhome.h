#ifndef HOMERIBBON_H
#define HOMERIBBON_H

#include "ribbonwidget.h"

namespace Ui {
class RibbonHome;
}

class RibbonHome : public RibbonWidget
{
    Q_OBJECT
    
public:
	explicit RibbonHome(QWidget *parent = 0);
	~RibbonHome();
    
private:
	Ui::RibbonHome *ui;
};

#endif // HOMERIBBON_H
