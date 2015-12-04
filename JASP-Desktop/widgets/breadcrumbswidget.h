#ifndef BREADCRUMBSWIDGET_H
#define BREADCRUMBSWIDGET_H

#include <QWidget>
#include <QBoxLayout>

class BreadCrumbsWidget : public QWidget
{
	Q_OBJECT
public:
	explicit BreadCrumbsWidget(QWidget *parent = 0);

signals:

public slots:

private:
	QHBoxLayout *_layout;
};

#endif // BREADCRUMBSWIDGET_H
