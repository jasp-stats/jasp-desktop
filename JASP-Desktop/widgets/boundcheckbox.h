#ifndef BOUNDCHECKBOX_H
#define BOUNDCHECKBOX_H

#include <QCheckBox>

#include "bound.h"
#include "options/optionboolean.h"

using namespace std;

class BoundCheckBox : public QCheckBox, public Bound
{
    Q_OBJECT
public:
	explicit BoundCheckBox(QWidget *parent = 0);
	virtual void bindTo(Option *option) OVERRIDE;

protected:
	virtual void nextCheckState() OVERRIDE;
	bool event(QEvent *e) OVERRIDE;

signals:
    
public slots:

private:
	OptionBoolean *_boundTo;

};

#endif // BOUNDCHECKBOX_H
