#ifndef VARIABLESWIDGET_H
#define VARIABLESWIDGET_H

#include <QWidget>

namespace Ui {
class VariablesWidget;
}

class VariablesWidget : public QWidget
{
	Q_OBJECT

public:
	explicit VariablesWidget(QWidget *parent = 0);
	~VariablesWidget();

private:
	Ui::VariablesWidget *ui;
};

#endif // VARIABLESWIDGET_H
