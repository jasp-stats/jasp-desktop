#ifndef RIBBONR11TLEARN_H
#define RIBBONR11TLEARN_H

#include "ribbonwidget.h"

namespace Ui {
class RibbonR11tLearn;
}

class RibbonR11tLearn : public RibbonWidget
{
	Q_OBJECT

public:
	explicit RibbonR11tLearn(QWidget *parent = 0);
	~RibbonR11tLearn();

private:
	Ui::RibbonR11tLearn *ui;
};

#endif // RIBBONR11TLEARN_H
