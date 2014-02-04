#ifndef RIBBONSEM_H
#define RIBBONSEM_H

#include "ribbonwidget.h"

namespace Ui {
class RibbonSEM;
}

class RibbonSEM : public RibbonWidget
{
	Q_OBJECT

public:
	explicit RibbonSEM(QWidget *parent = 0);
	~RibbonSEM();

private:
	Ui::RibbonSEM *ui;
};

#endif // RIBBONSEM_H
