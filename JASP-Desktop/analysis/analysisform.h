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
#include "utilities/qutils.h"



class ListModelTermsAssigned;
class BoundQMLItem;
class JASPControlBase;
class QMLExpander;

class AnalysisForm : public QQuickItem, public VariableInfoProvider
{
	Q_OBJECT
	Q_PROPERTY( QQuickItem * errorMessagesItem	READ errorMessagesItem	WRITE setErrorMessagesItem	NOTIFY errorMessagesItemChanged	)

public:
	explicit					AnalysisForm(QQuickItem * = nullptr);
				void			bindTo();
				void			unbind();

				void			runRScript(QString script, QString controlName, bool whiteListedVersion);
				void			refreshAnalysis();
				
				void			itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value) override;

				DataSetPackage *getDataSetPackage() const { return _package; }
				void			setMustBe(		std::set<std::string>						mustBe);
				void			setMustContain(	std::map<std::string,std::set<std::string>> mustContain);
					
public slots:
				void			runScriptRequestDone(const QString& result, const QString& requestId);
				void			dataSetChangedHandler();

signals:
				void			sendRScript(QString script, int key);
				void			formChanged(Analysis* analysis);
				void			formCompleted();
				void			dataSetChanged();
				void			refreshTableViewModels();
				void			errorMessagesItemChanged();


protected:
				QVariant		requestInfo(const Term &term, VariableInfo::InfoType info) const override;

public:
	ListModel*	getRelatedModel(QMLListView* listView)	{ return _relatedModelMap[listView]; }
	ListModel*	getModel(const QString& modelName)		{ return _modelMap[modelName]; }
	Options*	getAnalysisOptions()					{ return _analysis->options(); }
	JASPControlWrapper*	getControl(const QString& name)			{ return _controls[name]; }
	void		addListView(QMLListView* listView, QMLListView* sourceListView);
	void		clearErrors();
	QMLExpander* nextExpander(QMLExpander* expander)	{ return _nextExpanderMap[expander]; }

	Options*	options() { return _options; }
	void		addControl(JASPControlBase* control);

	Q_INVOKABLE void reset();
    Q_INVOKABLE void exportResults();
	Q_INVOKABLE void addError(const QString& message);

	void		refreshAvailableVariablesModels() { _setAllAvailableVariablesModel(true); }

	QQuickItem*	errorMessagesItem()		{ return _errorMessagesItem;	}
	GENERIC_SET_FUNCTION(ErrorMessagesItem, _errorMessagesItem, errorMessagesItemChanged, QQuickItem*)


protected:
	void		_setAllAvailableVariablesModel(bool refreshAssigned = false);


private:
	void		_addControlWrapper(JASPControlWrapper* controlWrapper);
	void		_setUpControls();
	void		_setUpRelatedModels();
	void		_setUpItems();
	void		_orderExpanders();
	void		_setErrorMessages();
	void		_cleanUpForm();
	void		setControlIsDependency(QString controlName, bool isDependency);
	void		setControlMustContain(QString controlName, QStringList containThis);
	void		setControlIsDependency(std::string controlName, bool isDependency)					{ setControlIsDependency(tq(controlName), isDependency);	}
	void		setControlMustContain(std::string controlName, std::set<std::string> containThis)	{ setControlMustContain(tq(controlName), tql(containThis)); }

private slots:
	void		formCompletedHandler();
	void		_formCompletedHandler();

protected:
	Analysis								*	_analysis			= nullptr;
	DataSetPackage							*	_package			= nullptr;
	Options									*	_options			= nullptr;
	QMap<QString, JASPControlWrapper* >			_controls;

	QVector<JASPControlWrapper*>				_orderedControls;
	QMap<QMLListView*, ListModel* >				_relatedModelMap;
	QMap<QString, ListModel* >					_modelMap;
	QVector<QMLExpander*>						_expanders;
	QMap<QMLExpander*, QMLExpander*>			_nextExpanderMap;
	bool										_removed = false;
	std::set<std::string>						_mustBe;
	std::map<std::string,std::set<std::string>>	_mustContain;
	
private:
	QQuickItem								*	_errorMessagesItem	= nullptr;
	std::vector<ListModelTermsAvailable*>		_allAvailableVariablesModels,
												_allAvailableVariablesModelsWithSource;
	QList<QString>								_errorMessages;
	long										_lastAddedErrorTimestamp = 0;

};

#endif // ANALYSISFORM_H
