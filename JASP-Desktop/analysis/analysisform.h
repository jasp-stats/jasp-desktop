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

#ifndef ANALYSISFORM_H
#define ANALYSISFORM_H

#include <QMap>

#include "dataset.h"
#include "options/bound.h"
#include "options/options.h"
#include "options/optionvariables.h"

#include "analysis/options/variableinfo.h"
#include "analysis.h"

#include <QQuickItem>

#include "analysis.h"
#include "boundqmlitem.h"
#include "widgets/listmodel.h"
#include "options/variableinfo.h"
#include "analysisqmldefines.h"
#include "widgets/listmodeltermsavailable.h"
#include "gui/messageforwarder.h"



class ListModelTermsAssigned;
class BoundQMLItem;

class AnalysisForm : public QQuickItem, public VariableInfoProvider
{
	Q_OBJECT

public:
	explicit					AnalysisForm(QQuickItem * = nullptr);
				void			bindTo(Options *options, DataSet *dataSet, const Json::Value& oldVersionOptions);
				void			unbind();

				bool			hasIllegalValue()		const;
				const QString	&illegalValueMessage()	const;
				void			illegalValueHandler(Bound *source);

				void			runRScript(QString script, QString controlName);
				
	virtual		void			itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value) OVERRIDE;
					
public slots:
				void			runScriptRequestDone(const QString& result, const QString& requestId);
				void			dataSetChanged();

signals:
				void			illegalChanged(AnalysisForm * form);
				void			sendRScript(QString script, int key);
				void			formChanged(Analysis* analysis);
				void			formCompleted();

protected:
				QVariant		requestInfo(const Term &term, VariableInfo::InfoType info) const override;

public:
	void		addError(const QString& error);

	ListModel*	getRelatedModel(QMLListView* listView)	{ return _relatedModelMap[listView]; }
	ListModel*	getModel(const QString& modelName)		{ return _modelMap[modelName]; }
	Options*	getAnalysisOptions()					{ return _analysis->options(); }
	QMLItem*	getControl(const QString& name)			{ return _controls[name]; }
	DataSet*	getDataSet()							{ return _dataSet; }
	void		addListView(QMLListView* listView, const std::map<QString, QString>& relationMap);

	Options*	options() { return _options; }

protected:
	void		_setAllAvailableVariablesModel(bool refreshAssigned = false);


private:
	void		_parseQML();
	void		_setUpRelatedModels(const std::map<QString, QString>& relationMap);
	void		_setUpItems();
	void		_setErrorMessages();

private slots:
	void		formCompletedHandler();
	void		_formCompletedHandler();

protected:
	Analysis								*_analysis;
	QMap<QString, QMLItem* >				_controls;
	QVector<QMLItem*>						_orderedControls;	
	std::map<QMLListView*, ListModel* >		_relatedModelMap;
	std::map<QString, ListModel* >			_modelMap;
	DataSet									*_dataSet;
	Options									*_options;

	OptionVariables							*_mainVariables;

	void									updateIllegalStatus();

	std::list<Bound *>						_bounds;
	bool									_hasIllegalValue;
	QString									_illegalMessage;
	bool									_removed = false;
	
private:
	std::vector<ListModelTermsAvailable*>	_allAvailableVariablesModels;
	QQuickItem								*_errorMessagesItem;
	QList<QString>							_errorMessages;
};

#endif // ANALYSISFORM_H
