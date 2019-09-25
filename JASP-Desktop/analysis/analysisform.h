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
				void			bindTo();
				void			unbind();

				void			runRScript(QString script, QString controlName, bool whiteListedVersion);
				void			refreshAnalysis();
				
				void			itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value) override;

				DataSetPackage *getDataSetPackage() const { return _package; }
					
public slots:
				void			runScriptRequestDone(const QString& result, const QString& requestId);
				void			dataSetChangedHandler();

signals:
				void			sendRScript(QString script, int key);
				void			formChanged(Analysis* analysis);
				void			formCompleted();
				void			dataSetChanged();
				void			refreshTableViewModels();

protected:
				QVariant		requestInfo(const Term &term, VariableInfo::InfoType info) const override;

public:
	void		addError(const QString& error);

	ListModel*	getRelatedModel(QMLListView* listView)	{ return _relatedModelMap[listView]; }
	ListModel*	getModel(const QString& modelName)		{ return _modelMap[modelName]; }
	Options*	getAnalysisOptions()					{ return _analysis->options(); }
	QMLItem*	getControl(const QString& name)			{ return _controls[name]; }
	void		addListView(QMLListView* listView, const std::map<QString, QString>& relationMap);
	void		clearErrors()							{ _errorMessages.clear(); _setErrorMessages(); }

	Options*	options() { return _options; }

	Q_INVOKABLE void reset();
    Q_INVOKABLE void exportResults();

	void		refreshAvailableVariablesModels() { _setAllAvailableVariablesModel(true); }

protected:
	void		_setAllAvailableVariablesModel(bool refreshAssigned = false);


private:
	void		_parseQML();
	void		_setUpRelatedModels(const std::map<QString, QString>& relationMap);
	void		_setUpItems();
	void		_setErrorMessages();
	void		_cleanUpForm();

private slots:
	void		formCompletedHandler();
	void		_formCompletedHandler();

protected:
	Analysis								*	_analysis			= nullptr;
	DataSetPackage							*	_package			= nullptr;
	Options									*	_options			= nullptr;
	QMap<QString, QMLItem* >					_controls;
	QVector<QMLItem*>							_orderedControls;
	std::map<QMLListView*, ListModel* >			_relatedModelMap;
	std::map<QString, ListModel* >				_modelMap;
	bool										_removed			= false;
	
private:
	QQuickItem								*	_errorMessagesItem	= nullptr;
	std::vector<ListModelTermsAvailable*>		_allAvailableVariablesModels,
												_allAvailableVariablesModelsWithSource;
	QList<QString>								_errorMessages;
};

#endif // ANALYSISFORM_H
