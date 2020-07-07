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
	Q_PROPERTY(QQuickItem * errorMessagesItem	READ errorMessagesItem	WRITE setErrorMessagesItem	NOTIFY errorMessagesItemChanged	)
	Q_PROPERTY(bool			needsRefresh		READ needsRefresh									NOTIFY needsRefreshChanged		)
	Q_PROPERTY(bool			hasVolatileNotes	READ hasVolatileNotes								NOTIFY hasVolatileNotesChanged	)
	Q_PROPERTY(bool			runOnChange			READ runOnChange		WRITE setRunOnChange		NOTIFY runOnChangeChanged )

public:
	explicit					AnalysisForm(QQuickItem * = nullptr);
				void			bindTo();
				void			unbind();

				void			runRScript(QString script, QString controlName, bool whiteListedVersion);
				
				void			itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value) override;

				void			setMustBe(		std::set<std::string>						mustBe);
				void			setMustContain(	std::map<std::string,std::set<std::string>> mustContain);

				bool			runOnChange()	{ return _runOnChange; }
				void			setRunOnChange(bool change);

				bool			runWhenThisOptionIsChanged(Option* option);
					
public slots:
				void			runScriptRequestDone(const QString& result, const QString& requestId);
				void			dataSetChangedHandler();
				void			dataSetColumnsChangedHandler();
				void			replaceVariableNameInListModels(const std::string & oldName, const std::string & newName);

signals:
				void			sendRScript(QString script, int key);
				void			formChanged(Analysis* analysis);
				void			formCompleted();
				void			dataSetChanged();
				void			refreshTableViewModels();
				void			errorMessagesItemChanged();
				void			languageChanged();
				void			needsRefreshChanged();
				void			hasVolatileNotesChanged();
				void			runOnChangeChanged();
				void			valueChanged(JASPControlBase* item);

protected:
				QVariant		requestInfo(const Term &term, VariableInfo::InfoType info) const override;

public:
	ListModel			*	getRelatedModel(QMLListView* listView)	{ return _relatedModelMap[listView]; }
	ListModel			*	getModel(const QString& modelName)		{ return _modelMap.count(modelName) > 0 ? _modelMap[modelName] : nullptr; } // Maps create elements if they do not exist yet
	Options				*	getAnalysisOptions()					{ return _analysis->options(); }
	JASPControlWrapper	*	getControl(const QString& name)			{ return _controls[name]; }
	void					addListView(QMLListView* listView, QMLListView* sourceListView);
	void					clearFormErrors();
	QMLExpander			*	nextExpander(QMLExpander* expander)		{ return _nextExpanderMap[expander]; }
	BoundQMLItem		*	getBoundItem(Option* option)			{ return _optionControlMap[option]; }

	Options				*	options() { return _options; }
	void					addControl(JASPControlBase* control);

	Q_INVOKABLE void reset();
    Q_INVOKABLE void exportResults();
	Q_INVOKABLE void addFormError(const QString& message);
	Q_INVOKABLE void refreshAnalysis();
	Q_INVOKABLE void runAnalysis();

	void		addControlError(JASPControlBase* control, QString message, bool temporary = false, bool warning = false);
	void		clearControlError(JASPControlBase* control);
	void		cleanUpForm();
	void		addControlErrorSet(JASPControlBase* control, bool add);
	void		addControlWarningSet(JASPControlBase* control, bool add);
	void		refreshAvailableVariablesModels() { _setAllAvailableVariablesModel(true); }

	QQuickItem*	errorMessagesItem()		{ return _formErrorMessagesItem;	}
	GENERIC_SET_FUNCTION(ErrorMessagesItem, _formErrorMessagesItem, errorMessagesItemChanged, QQuickItem*)

	bool		hasError() { return _jaspControlsWithErrorSet.size() > 0; }

	bool		isOwnComputedColumn(const QString& col)			const	{ return _computedColumns.contains(col); }
	void		addOwnComputedColumn(const QString& col)				{ _computedColumns.push_back(col); }
	void		removeOwnComputedColumn(const QString& col)				{ _computedColumns.removeAll(col); }

	bool		needsRefresh()		const;
	bool		hasVolatileNotes()	const;

protected:
	void		_setAllAvailableVariablesModel(bool refreshAssigned = false);


private:
	void		_addControlWrapper(JASPControlWrapper* controlWrapper);
	void		_setUpControls();
	void		_setUpRelatedModels();
	void		_setUpItems();
	void		_orderExpanders();
	void		_setErrorMessages();
	QString		_getControlLabel(JASPControlBase* boundControl);
	void		_addLoadingError();
	void		setControlIsDependency(QString controlName, bool isDependency);
	void		setControlMustContain(QString controlName, QStringList containThis);
	void		setControlIsDependency(std::string controlName, bool isDependency)					{ setControlIsDependency(tq(controlName), isDependency);	}
	void		setControlMustContain(std::string controlName, std::set<std::string> containThis)	{ setControlMustContain(tq(controlName), tql(containThis)); }
	QQuickItem* _getControlErrorMessageUsingThisJaspControl(JASPControlBase* jaspControl);

private slots:
	void		formCompletedHandler();
	void		_formCompletedHandler();

protected:
	Analysis								*	_analysis			= nullptr;
	Options									*	_options			= nullptr;
	QMap<QString, JASPControlWrapper* >			_controls;

	QVector<JASPControlWrapper*>				_orderedControls;
	QMap<Option*, BoundQMLItem*>				_optionControlMap;
	QMap<QMLListView*, ListModel* >				_relatedModelMap;
	QMap<QString, ListModel* >					_modelMap;
	QVector<QMLExpander*>						_expanders;
	QMap<QMLExpander*, QMLExpander*>			_nextExpanderMap;
	QMap<JASPControlWrapper*, QMLExpander*>		_controlExpanderMap;
	bool										_removed = false;
	std::set<std::string>						_mustBe;
	std::map<std::string,std::set<std::string>>	_mustContain;
	
private:
	QQuickItem								*	_formErrorMessagesItem	= nullptr;
	std::vector<ListModelTermsAvailable*>		_allAvailableVariablesModels,
												_allAvailableVariablesModelsWithSource;
	QList<QString>								_formErrorMessages;
	long										_lastAddedErrorTimestamp = 0;
	QQmlComponent*								_controlErrorMessageComponent = nullptr;
	QList<QQuickItem*>							_controlErrorMessageCache;
	QSet<JASPControlBase*>						_jaspControlsWithErrorSet;
	QSet<JASPControlBase*>						_jaspControlsWithWarningSet;
	QList<QString>								_computedColumns;
	bool										_runOnChange = true;
};

#endif // ANALYSISFORM_H
