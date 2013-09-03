#ifndef PROGRESSWIDGET_H
#define PROGRESSWIDGET_H

#include <QWidget>

namespace Ui {
class ProgressWidget;
}

class ProgressWidget : public QWidget
{
	Q_OBJECT
	
public:
	explicit ProgressWidget(QWidget *parent = 0);
	~ProgressWidget();

public slots:
	void setStatus(const QString status, int progress);
	
private:
	Ui::ProgressWidget *ui;
};

#endif // PROGRESSWIDGET_H
