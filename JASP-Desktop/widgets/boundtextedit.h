#ifndef BOUNDTEXTEDIT_H
#define BOUNDTEXTEDIT_H

#include <QTextEdit>
#include <QLabel>

#include "bound.h"
#include "common.h"
#include "textmodellavaan.h"

class BoundTextEdit : public QTextEdit, public Bound
{
	Q_OBJECT
public:
	explicit BoundTextEdit(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;

signals:

public slots:

private slots:
	void cursorPositionChangedHandler();
	void errorStateChangedHandler();
	void contentsChangedHandler();

protected:
	void keyPressEvent(QKeyEvent *event) OVERRIDE;
	void resizeEvent(QResizeEvent *e) OVERRIDE;
	void paintEvent(QPaintEvent *event) OVERRIDE;

private:

	QString _errorStylesheet;
	QString _okStylesheet;
	QString _okMessage;

	bool _applied;
	QLabel *_status;
	TextModelLavaan *_model;

};

#endif // BOUNDTEXTEDIT_H
