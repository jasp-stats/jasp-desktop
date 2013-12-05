#ifndef BOUNDGROUPBOX_H
#define BOUNDGROUPBOX_H

#include <QEvent>
#include <QWidget>
#include <QTimer>
#include <QButtonGroup>

#include "bound.h"
#include "options/optionlist.h"

class BoundGroupBox : public QWidget, public Bound
{
	Q_OBJECT
public:
	explicit BoundGroupBox(QWidget *parent = 0);
	void bindTo(Option *option) OVERRIDE;

protected:
	void childEvent(QChildEvent *child) OVERRIDE;
	
private:
	QButtonGroup *_buttonGroup;
	QTimer *_timer;
	OptionList *_option;

signals:
	
private slots:
	void updateGroup();
	void itemSelected(QAbstractButton *button);


	
};

#endif // BOUNDGROUPBOX_H
