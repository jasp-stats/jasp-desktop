#include "analysisform.h"

#include "boost/foreach.hpp"

#include "bound.h"

#include <QLabel>
#include <QTimer>
#include <QResizeEvent>
#include "widgets/boundlistview.h"
#include "widgets/boundpairstable.h"
#include "utils.h"

using namespace std;

AnalysisForm::AnalysisForm(QString name, QWidget *parent) :
	QWidget(parent),
	_availableVariablesModel(parent)
{
	setObjectName(name);
	_mainVariables = NULL;
}

void AnalysisForm::bindTo(Options *options, DataSet *dataSet)
{
	_dataSet = dataSet;

	vector<string> columnNames;

	BOOST_FOREACH(Column &column, dataSet->columns())
		columnNames.push_back(column.name());

	_availableVariablesModel.setInfoProvider(this);
	_availableVariablesModel.setVariables(columnNames);

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


}

void AnalysisForm::unbind()
{
	BOOST_FOREACH(const string &name, _options->names)
	{
		Option *option = _options->get(name);

		QString qsName = QString::fromUtf8(name.c_str(), name.length());
		qsName.replace('/', '_');

		QWidget *child = this->findChild<QWidget*>(qsName);

		Bound *boundChild = dynamic_cast<Bound*>(child);

		if (boundChild != NULL)
			boundChild->unbind();
	}
}

QVariant AnalysisForm::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	if (info == VariableInfo::VariableType)
	{
		return _dataSet->column(term.asString()).columnType();
	}
	else if (info == VariableInfo::Labels)
	{
		QStringList values;
		Labels &labels = _dataSet->column(term.asString()).labels();
		for (uint i = 0; i < labels.size(); i++)
			values.append(tq(labels.at(i).text()));

		return values;
	}
	else
	{
		return QVariant();
	}
}

