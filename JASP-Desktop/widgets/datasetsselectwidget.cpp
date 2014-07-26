
#include "datasetsselectwidget.h"

#include "datasetselectwidget.h"

#include <QDir>

DataSetsSelectWidget::DataSetsSelectWidget(QWidget *parent) :
	QWidget(parent)
{
	_layout = new QGridLayout(this);
}

void DataSetsSelectWidget::addDataSetOption(QString path)
{
	int index = path.lastIndexOf("/");
	if (index != -1)
	{
		QString name = path.mid(index + 1);
		QString description = path.mid(0, index);
		addDataSetOption(path, name, description);
	}
	else
	{
		addDataSetOption(path, path, "");
	}
}

void DataSetsSelectWidget::addDataSetOption(QString path, QString dataSetName, QString dataSetDescription)
{
	if (_children.size() >= 5)
	{
		QWidget *last = _children.last();
		_children.removeLast();
		delete last;
	}

	for (int i = 0; i < _children.size(); i++)
		_layout->removeWidget(_children.at(i));

	DataSetSelectWidget *select = new DataSetSelectWidget(this);
	select->setDataSetName(dataSetName);
	select->setDataSetDescription(dataSetDescription);
	select->setDataSetPath(path);
	_children.append(select);

	connect(select, SIGNAL(dataSetSelected(QString)), this, SLOT(dataSetSelectedHandler(QString)));

	for (int i = 0; i < _children.size(); i++)
		_layout->addWidget(_children.at(i), i, 0);

}

void DataSetsSelectWidget::setDataSets(const QStringList &options)
{
	clearDataSets();
	foreach (const QString &path, options)
		addDataSetOption(path);
}

void DataSetsSelectWidget::clearDataSets()
{
	while (_children.size() > 0)
	{
		QWidget *widget = _children.first();
		_children.removeFirst();
		delete widget;
	}
}

void DataSetsSelectWidget::dataSetSelectedHandler(const QString &path)
{
	emit dataSetSelected(path);
}
