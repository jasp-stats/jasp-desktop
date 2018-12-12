//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "options/bound.h"
#include "utilities/qutils.h"


using namespace std;

int AnalysisForm::_scriptRequestCounter = 0;

AnalysisForm::AnalysisForm(QString name, QObject *parent) :
	QObject(parent),
	_availableVariablesModel(this)
{
	setObjectName(name);
	_mainVariables = NULL;

	_options = NULL;
	_dataSet = NULL;

	_hasIllegalValue = false;
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
	try {

		if (info == VariableInfo::VariableType)
		{
			return _dataSet->column(term.asString()).columnType();
		}
		else if (info == VariableInfo::Labels)
		{
			QStringList values;
			Labels &labels = _dataSet->column(term.asString()).labels();
			for (Labels::const_iterator label_it = labels.begin(); label_it != labels.end(); ++label_it)
				values.append(tq(label_it->text()));

			return values;
		}
	}
	catch(columnNotFound e) {} //just return an empty QVariant right?
	catch(std::exception e)
	{
#ifdef JASP_DEBUG
		std::cout << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
#endif
		throw e;
	}
	return QVariant();

}

void AnalysisForm::setVariablesModel()
{
	vector<string> columnNames;

	if (_dataSet != NULL)
	{
		for (Column &column: _dataSet->columns())
			columnNames.push_back(column.name());
	}

	_availableVariablesModel.setInfoProvider(this);
	_availableVariablesModel.setVariables(columnNames);
}

void AnalysisForm::updateIllegalStatus()
{
	QString message;
	bool illegal = false;

	for (const Bound *bound : _bounds)
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

		emit illegalChanged(this);
	}
}

void AnalysisForm::illegalValueHandler(Bound *source)
{
	updateIllegalStatus();
}

void AnalysisForm::runRScript(QString script, QVariant key)
{
	int newRequestId = _scriptRequestCounter++;
	_scriptRequestIdToKey[newRequestId] = key;
	
	emit sendRScript(script, newRequestId);
}

void AnalysisForm::runScriptRequestDone(const QString & result, int requestId)
{
	if(!runRScriptRequestedForId(requestId)) return;	

	QVariant key = _scriptRequestIdToKey[requestId];
	_scriptRequestIdToKey.erase(requestId);
	
	rScriptDoneHandler(key, result);
}

void AnalysisForm::rScriptDoneHandler(QVariant key, const QString & result) 
{ 
	throw std::runtime_error("runRScript done but handler not implemented!\nImplement an override for RScriptDoneHandler\n");	
}

bool AnalysisForm::runRScriptRequestedForId(int requestId) 
{ 
	return _scriptRequestIdToKey.count(requestId) > 0; 
}
