//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "analysisform.h"

#include <QLabel>
#include <QTimer>
#include <QResizeEvent>
#include <QDebug>

#include <boost/bind.hpp>
#include <boost/foreach.hpp>

#include "bound.h"
#include "qutils.h"
#include "widgets/boundlistview.h"
#include "widgets/boundpairstable.h"

using namespace std;

AnalysisForm::AnalysisForm(QString name, QWidget *parent) :
	QWidget(parent),
	_availableVariablesModel(parent)
{
	setObjectName(name);
	_mainVariables = NULL;

	_options = NULL;
	_dataSet = NULL;

	_hasIllegalValue = false;
}

void AnalysisForm::bindTo(Options *options, DataSet *dataSet)
{
	if (_options != NULL)
		unbind();

	_dataSet = dataSet;

	vector<string> columnNames;

	if (_dataSet != NULL)
	{
		BOOST_FOREACH(Column &column, dataSet->columns())
			columnNames.push_back(column.name());
	}

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
		{
			_bounds.push_back(boundChild);
			boundChild->bindTo(option);
			boundChild->illegalChanged.connect(boost::bind(&AnalysisForm::illegalValueHandler, this, _1));
		}
		else
		{
			qDebug() << "child not found : " << qsName << " in AnalysisForm::setOptions()";
		}
	}

	updateIllegalStatus();
}

void AnalysisForm::unbind()
{
	_bounds.clear();
	updateIllegalStatus();

	if (_options == NULL)
		return;

	BOOST_FOREACH(const string &name, _options->names)
	{
		QString qsName = QString::fromUtf8(name.c_str(), name.length());
		qsName.replace('/', '_');

		QWidget *child = this->findChild<QWidget*>(qsName);

		Bound *boundChild = dynamic_cast<Bound*>(child);

		if (boundChild != NULL)
			boundChild->unbind();
	}

	_options = NULL;
}

bool AnalysisForm::hasIllegalValue() const
{
	return _hasIllegalValue;
}

const QString &AnalysisForm::illegalValueMessage() const
{
	return _illegalMessage;
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
		for (Labels::const_iterator label_it = labels.begin(); label_it != labels.end(); ++label_it)
		{
			values.append(tq(label_it->text()));
		}

		return values;
	}
	else
	{
		return QVariant();
	}
}

void AnalysisForm::updateIllegalStatus()
{
	QString message;
	bool illegal = false;

	foreach (Bound *bound, _bounds)
	{
		if (bound->isIllegal())
		{
			if ( ! illegal)
				message = bound->illegalMessage();

			illegal = true;
		}
	}

	if (illegal != _hasIllegalValue || message != _illegalMessage)
	{
		_hasIllegalValue = illegal;
		_illegalMessage = message;

		emit illegalChanged();
	}
}

void AnalysisForm::illegalValueHandler(Bound *source)
{
	updateIllegalStatus();
}

