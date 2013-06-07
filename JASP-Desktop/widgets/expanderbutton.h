#ifndef EXPANDERBUTTON_H
#define EXPANDERBUTTON_H

#include <QPushButton>

class ExpanderButton : public QPushButton
{
	Q_OBJECT
public:
	explicit ExpanderButton(QWidget *parent = 0);

protected:
	virtual void nextCheckState() override;

private:
	bool _expanded = false;
	QIcon _expandedIcon = QIcon(QString(":/images/expander-arrow-down.png"));
	QIcon _contractedIcon = QIcon(QString(":/images/expander-arrow-up.png"));
	
};

#endif // EXPANDERBUTTON_H
