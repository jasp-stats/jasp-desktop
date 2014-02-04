#ifndef BOUNDTEXTEDIT_H
#define BOUNDTEXTEDIT_H

#include <QPlainTextEdit>

#include "bound.h"
#include "common.h"
#include "options/optionstring.h"

class BoundTextEdit : public QPlainTextEdit, public Bound
{
	Q_OBJECT
public:
	explicit BoundTextEdit(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;

signals:

public slots:

protected:
	void keyPressEvent(QKeyEvent *event) OVERRIDE;

private:
	OptionString *_boundTo;

};

#endif // BOUNDTEXTEDIT_H
