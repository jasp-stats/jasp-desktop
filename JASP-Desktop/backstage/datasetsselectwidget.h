#ifndef DATASETSSELECTWIDGET_H
#define DATASETSSELECTWIDGET_H

#include <QWidget>
#include <QGridLayout>
#include <QButtonGroup>
#include <QAbstractButton>
#include <QPaintEvent>

#include "common.h"

class DataSetsSelectWidget : public QAbstractButton
{
	Q_OBJECT
public:
	explicit DataSetsSelectWidget(QWidget *parent = 0);

	void addDataSetOption(QString path);
	void addDataSetOption(QString path, QString dataSetName, QString dataSetDescription);
	void setDataSets(const QStringList &options);
	void clearDataSets();

signals:

	void dataSetOpened(const QString &path);

protected:
	virtual void paintEvent(QPaintEvent *event) OVERRIDE;

private slots:
	void dataSetOpenedHandler(const QString &path);

private:
	QButtonGroup *_buttons;
	QGridLayout *_layout;
	QList<QWidget*> _children;

};

#endif // DATASETSSELECTWIDGET_H
