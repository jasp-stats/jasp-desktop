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
#include <QQuickItem>

#include "options/boundcontrol.h"
#include "options/options.h"
#include "options/optionvariables.h"

#include "analysis/options/variableinfo.h"
#include "analysis.h"
#include "widgets/listmodel.h"
#include "options/variableinfo.h"
#include "widgets/listmodeltermsavailable.h"
#include "gui/messageforwarder.h"
#include "utilities/qutils.h"



class ListModelTermsAssigned;
class JASPControl;
class ExpanderButtonBase;
class ColumnsModel;

class AnalysisForm : public QQuickItem, public VariableInfoProvider
{
	Q_OBJECT
	Q_PROPERTY(QString		errors				READ errors											NOTIFY errorsChanged			)
	Q_PROPERTY(QString		warnings			READ warnings										NOTIFY warningsChanged			)
	Q_PROPERTY(bool			needsRefresh		READ needsRefresh									NOTIFY needsRefreshChanged		)
	Q_PROPERTY(bool			hasVolatileNotes	READ hasVolatileNotes								NOTIFY hasVolatileNotesChanged	)
	Q_PROPERTY(bool			runOnChange			READ runOnChange		WRITE setRunOnChange		NOTIFY runOnChangeChanged		)
	Q_PROPERTY(QString		info				READ info				WRITE setInfo				NOTIFY infoChanged				)
	Q_PROPERTY(QString		helpMD				READ helpMD											NOTIFY helpMDChanged			)
	Q_PROPERTY(QVariant		analysis			READ analysis			WRITE setAnalysis			NOTIFY analysisChanged			)

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
				void			setInfo(QString info);
				void			setAnalysis(QVariant analysis);
				void			rSourceChanged(const QString& name);


signals:
				void			sendRScript(QString script, int key);
				void			formChanged(Analysis* analysis);
				void			formCompleted();
				void			refreshTableViewModels();
				void			errorMessagesItemChanged();
				void			languageChanged();
				void			needsRefreshChanged();
				void			hasVolatileNotesChanged();
				void			runOnChangeChanged();
				void			valueChanged(JASPControl* item);
				void			infoChanged();
				void			helpMDChanged();
				void			errorsChanged();
				void			warningsChanged();
				void			analysisChanged();

protected:
				QVariant		requestInfo(const Term &term, VariableInfo::InfoType info)	const override;

public:
	ListModel			*	getModel(const QString& modelName)								const	{ return _modelMap.value(modelName);	} // Maps create elements if they do not exist yet
	void					addModel(ListModel* model)												{ if (!model->name().isEmpty())	_modelMap[model->name()] = model;			}
	Options				*	getAnalysisOptions()													{ return _analysis->options();												}
	JASPControl			*	getControl(const QString& name)											{ return _controls.contains(name) ? _controls[name] : nullptr;				}
	void					addListView(JASPListControl* listView, JASPListControl* sourceListView);
	ExpanderButtonBase	*	nextExpander(ExpanderButtonBase* expander)								{ return _nextExpanderMap[expander];										}
	JASPControl			*	getControl(Option* option)												{ return _optionControlMap[option];											}

	Options				*	options() { return _options; }
	void					addControl(JASPControl* control);

	Q_INVOKABLE void		clearFormErrors();
	Q_INVOKABLE void		clearFormWarnings();
	Q_INVOKABLE void		reset();
	Q_INVOKABLE void		exportResults();
	Q_INVOKABLE void		addFormError(const QString& message);
	Q_INVOKABLE void		refreshAnalysis();
	Q_INVOKABLE void		runAnalysis();

	void		addControlError(JASPControl* control, QString message, bool temporary = false, bool warning = false);
	void		clearControlError(JASPControl* control);
	void		cleanUpForm();
	bool		hasError();

	bool		isOwnComputedColumn(const QString& col)			const	{ return _computedColumns.contains(col); }
	void		addOwnComputedColumn(const QString& col)				{ _computedColumns.push_back(col); }
	void		removeOwnComputedColumn(const QString& col)				{ _computedColumns.removeAll(col); }

	bool		needsRefresh()		const;
	bool		hasVolatileNotes()	const;

	QString		info()				const { return _info; }
	QString		helpMD()			const;
	QString		metaHelpMD()		const;
	QString		errors()			const {	return msgsListToString(_formErrors);	}
	QString		warnings()			const { return msgsListToString(_formWarnings);	}
	QVariant	analysis()			const { return QVariant::fromValue(_analysis);	}
	void		addRSource(const QString& name, ListModel* model)	{ _rSourceModelMap[name] = model; }

	std::vector<std::string>	getValuesFromRSource(const QString& sourceID) { if (_analysis) return _analysis->getValuesFromRSource(sourceID); else return {}; }

protected:
	QString		msgsListToString(const QStringList & list) const;

private:
	void		_setUpControls();
	void		_setUpModels();
	void		_setUp();
	void		_orderExpanders();
	QString		_getControlLabel(QString controlName);
	void		_addLoadingError(QStringList wrongJson);
	void		setControlIsDependency(	QString controlName, bool isDependency);
	void		setControlMustContain(	QString controlName, QStringList containThis);
	void		setControlIsDependency(	std::string controlName, bool isDependency)					{ setControlIsDependency(tq(controlName), isDependency);	}
	void		setControlMustContain(	std::string controlName, std::set<std::string> containThis)	{ setControlMustContain(tq(controlName), tql(containThis)); }
	QQuickItem* _getControlErrorMessageOfControl(JASPControl* jaspControl);
	void		setAnalysisUp();

private slots:
	   void		formCompletedHandler();
	   void		_formCompletedHandler();
	   void		knownIssuesUpdated();

protected:
	Analysis								*	_analysis			= nullptr;
	Options									*	_options			= nullptr;
	QMap<QString, JASPControl* >				_controls;

	///Ordered on dependencies within QML, aka an assigned variables list depends on the available list it is connected to.
	QVector<JASPControl*>						_dependsOrderedCtrls;
	QMap<Option*, JASPControl*>					_optionControlMap;
	QMap<QString, ListModel* >					_modelMap;
	QVector<ExpanderButtonBase*>				_expanders;
	QMap<ExpanderButtonBase*, ExpanderButtonBase*>	_nextExpanderMap;
	QMap<JASPControl*, ExpanderButtonBase*>		_controlExpanderMap;
	bool										_removed = false;
	std::set<std::string>						_mustBe;
	std::map<std::string,std::set<std::string>>	_mustContain;
	
private:
	QStringList									_formErrors,
												_formWarnings;

	QQmlComponent*								_controlErrorMessageComponent = nullptr;
	QList<QQuickItem*>							_controlErrorMessageCache;
	QList<QString>								_computedColumns;
	bool										_runOnChange	= true,
												_formCompleted = false;
	QString										_info;
	QMap<QString, ListModel*>					_rSourceModelMap;

};

#endif // ANALYSISFORM_H
