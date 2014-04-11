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

	_options = options;

	BOOST_FOREACH(const string &name, options->names)
	{
		Option *option = options->get(name);

		QString qsName = QString::fromUtf8(name.c_str(), name.length());
		qsName.replace('/', '_');

		QWidget *child = this->findChild<QWidget*>(qsName);

		Bound *boundChild = dynamic_cast<Bound*>(child);

		if (boundChild != NULL)
			boundChild->bindTo(option);
		else
			qDebug() << "child not found : " << qsName << " in AnalysisForm::setOptions()";
	}

	_availableFields.setVariables(columnDescs);
}

