#ifndef DATASETSSELECTWIDGET_H
#define DATASETSSELECTWIDGET_H

#include <QWidget>
#include <QGridLayout>

class DataSetsSelectWidget : public QWidget
{
	Q_OBJECT
public:
	explicit DataSetsSelectWidget(QWidget *parent = 0);

	void addDataSetOption(QString path);
	void addDataSetOption(QString path, QString dataSetName, QString dataSetDescription);
	void setDataSets(const QStringList &options);
	void clearDataSets();

signals:

	void dataSetSelected(const QString &path);

private slots:
	void dataSetSelectedHandler(const QString &path);

private:
	QGridLayout *_layout;
	QList<QWidget*> _children;

};

#endif // DATASETSSELECTWIDGET_H
