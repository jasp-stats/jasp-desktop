#ifndef DATASETSELECTWIDGET_H
#define DATASETSELECTWIDGET_H

#include <QWidget>
#include <QPushButton>

#include "common.h"

namespace Ui {
class DataSetSelectWidget;
}

class DataSetSelectWidget : public QPushButton
{
	Q_OBJECT

public:
	explicit DataSetSelectWidget(QWidget *parent = 0);
	~DataSetSelectWidget();

	void setDataSetName(QString name);
	void setDataSetPath(QString path);
	void setDataSetDescription(QString description);

protected:
	bool eventFilter(QObject *object, QEvent *event) OVERRIDE;

signals:
	void dataSetSelected(const QString &filename);

private slots:
	void clickedHandler();

private:
	Ui::DataSetSelectWidget *ui;

	QString _path;
};

#endif // DATASETSELECTWIDGET_H
