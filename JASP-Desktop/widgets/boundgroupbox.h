#ifndef BOUNDGROUPBOX_H
#define BOUNDGROUPBOX_H

#include <QEvent>
#include <QWidget>
#include <QTimer>
#include <QButtonGroup>
#include <QRadioButton>
#include <QGroupBox>

#include "bound.h"
#include "itemmodelselectitem.h"
#include "groupbox.h"

class BoundGroupBox : public GroupBox, public Bound
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
	ItemModelSelectItem _model;

signals:
	
private slots:
	void updateGroup();
	void itemSelected(QAbstractButton *);
	
};

#endif // BOUNDGROUPBOX_H
