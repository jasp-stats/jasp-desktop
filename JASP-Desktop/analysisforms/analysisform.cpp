#include "analysisform.h"

#include "boost/foreach.hpp"

#include "bound.h"

#include <QLabel>
#include <QTimer>
#include <QResizeEvent>
#include "widgets/boundlistview.h"
#include "widgets/boundpairstable.h"

using namespace std;

AnalysisForm::AnalysisForm(QString name, QWidget *parent) :
	QWidget(parent),
	_availableFields(parent)
{
	setObjectName(name);
	_mainFields = NULL;
}

void AnalysisForm::set(Options *options, DataSet *dataSet)
{
	_dataSet = dataSet;

	QList<QPair<QString, int> > columnDescs;

	BOOST_FOREACH(Column &column, dataSet->columns())
	{
		string name = column.name();
		QString asQS = QString::fromUtf8(name.c_str(), name.length());
		int columnType = column.columnType();
		columnDescs.append(QPair<QString, int>(asQS, columnType));
	}

	_availableFields.setVariables(columnDescs);

	_options = options;

	BOOST_FOREACH(Option *option, *options)
	{
		QString name = QString::fromUtf8(option->name().c_str(), option->name().length());
		name.replace('/', '_');

		QWidget *child = this->findChild<QWidget*>(name);

		Bound *boundChild = dynamic_cast<Bound*>(child);

		if (boundChild != NULL)
		{
			boundChild->bindTo(option);
			boundChild->setDataSet(dataSet);
		}
		else
		{
			qDebug() << "child not found : " << name << " in AnalysisForm::setOptions()";
		}
	}
}

