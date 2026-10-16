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
#include <QPointer>
#include <QQuickItem>

#include "boundcontrols/boundcontrol.h"
#include "analysisbase.h"
#include "models/listmodel.h"
#include "qutils.h"
#include <queue>

class ListModelTermsAssigned;
class JASPControl;
class ExpanderButtonBase;
class RSyntax;
class Filter;

///
/// The backend for the `Form{}` used in all JASP's well, qml forms
/// This is directly a QQuickItem and connects to Analyses to inform it when the options change.
/// Each element placed inside the Form will be a derivative of JASPControl
/// A JASPControl can be also a BoundControl: it is bound in the sense that any changes to its values will be propagated to Analysis and stored there through `Analysis::setBoundValue`
/// Each time a JASPControl is initialized, it is registered by the form, and when the form is initialized it sets up the JASPControls either by their default values, 
/// or by the values stored in a JASP file (when a JASP file is loaded).
/// The interface errors and warnings are managed by this class.
class AnalysisForm : public QQuickItem
{
	friend class TestAll; ///< mds-plan D3.5: flip _initialized to simulate attach completion in the C++ harness (pre-attach deferral pins)
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY(QString			title					READ title					WRITE setTitle					NOTIFY titleChanged					)
	Q_PROPERTY(QString			errors					READ errors													NOTIFY errorsChanged				)
	Q_PROPERTY(QString			warnings				READ warnings												NOTIFY warningsChanged				)
	Q_PROPERTY(bool				needsRefresh			READ needsRefresh											NOTIFY needsRefreshChanged			)
	Q_PROPERTY(bool				hasVolatileNotes		READ hasVolatileNotes										NOTIFY hasVolatileNotesChanged		)
	Q_PROPERTY(bool				runOnChange				READ runOnChange			WRITE setRunOnChange			NOTIFY runOnChangeChanged			)
	Q_PROPERTY(QString			info					READ info					WRITE setInfo					NOTIFY infoChanged					)
	Q_PROPERTY(QString			infoBottom				READ infoBottom				WRITE setInfoBottom				NOTIFY infoBottomChanged			)
	Q_PROPERTY(QString			helpMD					READ helpMD													NOTIFY helpMDChanged				)
	Q_PROPERTY(QVariant			analysis				READ analysis												NOTIFY analysisChanged				)
	Q_PROPERTY(QVariantList		optionNameConversion	READ optionNameConversion	WRITE setOptionNameConversion	NOTIFY optionNameConversionChanged	)
	Q_PROPERTY(bool				showRButton				READ showRButton											NOTIFY showRButtonChanged			)
	Q_PROPERTY(bool				developerMode			READ developerMode											NOTIFY developerModeChanged			)
	Q_PROPERTY(QString			rSyntaxText				READ rSyntaxText											NOTIFY rSyntaxTextChanged			)
	Q_PROPERTY(bool				showAllROptions			READ showAllROptions		WRITE setShowAllROptions		NOTIFY showAllROptionsChanged		)
	Q_PROPERTY(bool				isAnnotated				READ isAnnotated											NOTIFY isAnnotatedChanged			)
	Q_PROPERTY(bool			relaxInputConstraints	READ relaxInputConstraints	WRITE setRelaxInputConstraints	NOTIFY relaxInputConstraintsChanged)
	Q_PROPERTY(QString			rSyntaxControlName		MEMBER rSyntaxControlName	CONSTANT															)
	Q_PROPERTY(JASPControl	*	activeJASPControl		READ getActiveJASPControl									NOTIFY activeJASPControlChanged		)
	Q_PROPERTY(Filter		*	filter					READ filter													NOTIFY filterChanged				)
	Q_PROPERTY(VariableInfo	*	varInfo					READ varInfo												CONSTANT							)
public:
	explicit				AnalysisForm(QQuickItem * = nullptr);
							~AnalysisForm();

	void					bindTo(const Json::Value & defaultOptions);

	// ---- mds-plan C11/D3.1: JSON generated on demand from the components ---------------------
	///< Recursively aggregate every registered bound control's generated option value into an
	/// options tree along the controls' bindTo paths: walks _dependsOrderedCtrls exactly like
	/// bindTo does (so a row child's parent row has been generated before the child is placed),
	/// places values through AnalysisBase::resolveOptionParent (the mirror's own path code) and,
	/// for Engine, attaches the send-time `.meta` tree (BoundControl::createEngineMeta - C8: no
	/// dataSetId/filterId, every other meta key flows from the family's createMeta) and the
	/// transient `.filterIds` sidecar tree (BoundControl::createFilterIds, mds-plan D3.2) at the
	/// same parent paths the mirror uses. Storage omits both trees and carries term filterIds
	/// inside the term options themselves (D5.1 preview).
	/// This is the machinery that replaces reading _boundValues at the consumer flip (D3.3/D3.5);
	/// until then tst_generatedOptionsJson.qml pins generator == mirror (modulo the intentional
	/// deltas documented there).
	Json::Value				createOptionsJson(BoundControl::JsonFormat format);
	///< QML/test surface: 0 = Engine, 1 = Storage, canonical (key-sorted) JSON string.
	Q_INVOKABLE QString		createOptionsJsonAsJson(int format);

	void					runRScript(	const QString & script, const QString & controlName, bool whiteListedVersion);
	void					runFilter(	const QString & name);
	
	Filter				*	filter();
	VariableInfo		*	varInfo() { return _varInfo; }

	void					itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value) override;

	void					setMustBe(		stringset						mustBe);
	void					setMustContain(	std::map<std::string,stringset> mustContain);

	bool					runOnChange()	{ return _runOnChange; }
	void					setRunOnChange(bool change);
	void					blockValueChangeSignal(bool block, bool notifyOnceUnblocked = true);
	QString					title()							const	{ return _analysis ? tq(_analysis->title())						: "";		}
	QString					titleDefault()					const	{ return _analysis ? tq(_analysis->titleDefault())				: "";		}
	QString					name()							const	{ return _analysis ? tq(_analysis->name())						: "";		}
	QString					module()						const	{ return _analysis ? tq(_analysis->module())					: "";		}
	QString					version()						const	{ return _analysis ? tq(_analysis->moduleVersion().asString())	: "";		}
	bool					hasVolatileNotes()				const	{ return _hasVolatileNotes;													}
	bool					wasUpgraded()					const	{ return _analysis ? _analysis->wasUpgraded() : false;						}
	bool					formCompleted()					const	{ return _formCompleted;	}
	bool					showRButton()					const	{ return _showRButton;		}
	bool					developerMode()					const	{ return _developerMode;	}
	QString					rSyntaxText()					const;
	bool					showAllROptions()				const;
	bool					isAnnotated()					const;
	void					setIsAnnotated(bool isAnnotated = true);
	bool					relaxInputConstraints()			const;
	void					setRelaxInputConstraints(	bool					relax);
	Json::Value				optionMeta(bool includeDescriptions = true)	const;

public slots:
	void					runScriptRequestDone(		const QString		&	result, const QString & requestId, bool hasError);
	void					filterByNameDone(			int dataSetID,	const QString		&	name,	const QString & error);
	void					boundValueChangedHandler(	JASPControl			*	control);
	void					setOptionNameConversion(	const QVariantList	&	conv);
	void					setTitle(					QString					title);
	void					setShowRButton(				bool					showRButton);
	void					setDeveloperMode(			bool					developerMode);
	void					setShowAllROptions(			bool					showAllROptions);
	void					sendRSyntax(				QString					text);
	void					toggleRSyntax();

signals:
	void					formChanged(				AnalysisBase	*	analysis);
	void					formCompletedSignal();
	void					refreshTableViewModels();
	void					errorMessagesItemChanged();
	void					languageChanged();
	void					needsRefreshChanged();
	void					hasVolatileNotesChanged();
	void					runOnChangeChanged();
	void					infoChanged();
	void					infoBottomChanged();
	void					helpMDChanged();
	void					errorsChanged();
	void					warningsChanged();
	void					analysisChanged();
	void					rSourceChanged(const QString& name);
	void					optionNameConversionChanged();
	void					titleChanged();
	void					filterChanged(Filter *);
	void					showRButtonChanged();
	void					developerModeChanged();
	void					rSyntaxTextChanged();
	void					showAllROptionsChanged();
	void					activeJASPControlChanged();
	void					isAnnotatedChanged();
	void					relaxInputConstraintsChanged(bool relax);
	///< mds-plan D2.4: the selection-HOST registry changed (a host registered or dropped out -
	///  eager unregister on destruction included). AnalysisBase re-reads its provider aggregate.
	void					selectionHostsChanged();
	///< mds-plan D2.4/D2.5: a registered host moved to another filter (HostFilterSelection has
	///  restamped its controls and notifies through notifySelectionHostFilterChanged).
	void					selectionHostFilterChanged(JASPControl * host);
		
public:
	ListModel			*	getModel(const QString& modelName)								const	{ return _modelMap.count(modelName) > 0 ? _modelMap[modelName] : nullptr;	} // Maps create elements if they do not exist yet
	void					addModel(ListModel* model)												{ if (!model->name().isEmpty())	_modelMap[model->name()] = model;			}
Q_INVOKABLE JASPControl	*	getControl(const QString& name)											{ return _controls.contains(name) ? _controls[name] : nullptr;				}
	void					addListView(JASPListControl* listView, JASPListControl* sourceListView);
	void					addControl(JASPControl* control);

	Q_INVOKABLE void		clearFormErrors();
	Q_INVOKABLE void		clearFormWarnings();
	Q_INVOKABLE void		reset();
	Q_INVOKABLE void		exportResults();
	Q_INVOKABLE void		addFormError(const QString& message);
	Q_INVOKABLE void		addFormWarning(const QString& message);
	Q_INVOKABLE void		refreshAnalysis();
	Q_INVOKABLE QVariant    getConstant(QString key, QVariant defaultValue) const;
	Q_INVOKABLE QVariant    getConstant(QString key, QVariant defaultValue, QString module, QString analysis) const;
	Q_INVOKABLE bool		initialized()			const	{ return _initialized; }
	Q_INVOKABLE QVariant	options()				const;
	Q_INVOKABLE void		setOptions(const QVariantMap& options);
	QString					generateWrapper(const QString& moduleName, const QString& analysisName, const QString& qmlFileName, const QString& analysisTitle, bool preloadData, bool multiDataSetAware = false);
	bool					parseOptions(std::string rawOptions, Json::Value& parsedOptions, std::string& errorMsg);
	void					setAnalysis(AnalysisBase *	analysis);
	void					addControlError(JASPControl* control, QString message, bool temporary = false, bool warning = false, bool closeable = true);
	void					clearControlError(JASPControl* control);
	void					clearAllErrors();
	void					cleanUpForm();
	bool					hasError();

	// --- selection-host registry (mds-plan C4/REGISTRY, M9, D2.3) --------------------------------
	// The syntax bridge must translate the surface strings of dataset selection options before
	// anything binds, and it used a findChildren scan for that - blind to hosts instantiated
	// asynchronously (ComponentsList rows). Selection HOSTS - the lists with a dataSetSelection-
	// Option (VariablesListBase, mds-plan C10/D2.2) and the TextAreaBase hosts - register here
	// (HostFilterSelection::setOption, or the first analysisBecameKnown() when the form was not
	// yet known at property time); destruction unregisters eagerly (and dead entries are pruned
	// at every read). A host registering LATE while the raw options of the current parse are
	// still pending resolves and applies its own option immediately - no scheduled "wait for
	// instantiation" passes anywhere.
	// Ordering guarantee (mds-plan D2.3): every control that resolves through a host carries a
	// bindTo dependency on that host's FilterSelect (derived from the varInfo resolution in
	// AnalysisForm::sortControls via JASPControl::selectionDependency), so each host's selection
	// is bound BEFORE the controls it serves - per host, independent of the other hosts.
	// This registry is the foundation of mds-plan D2.4 (AnalysisBase as VariableInfoProvider):
	// the provider aggregates over selectionHostControls(), reading each host's filter through
	// JASPControl::selectedSelectionFilter() - ids/filters re-resolved at use, never cached
	// encoder pointers (see the virtual's doc).

	///< Live registry entries (deduplicated, dead hosts pruned); read by the syntax bridge.
	Q_INVOKABLE QStringList	dataSetSelectionOptionNames() const;
	///< Live selection hosts (dead pruned, deduplicated) in registration order - D2.4's input.
	QList<JASPControl *>	selectionHostControls() const;
	///< Introspection (QML/tests, mds-plan D2.3): how many live hosts own a selection right now.
	Q_INVOKABLE int			selectionHostCount() const;
	///< Introspection: the live host controls themselves (JASPControl* items; selecting ones
	///  answer ownedSelectionVarInfo()/selectedSelectionFilter()).
	Q_INVOKABLE QVariantList selectionHosts() const;
	void					registerDataSetSelectionOption(const QString &option, JASPControl *host);
	void					unregisterDataSetSelectionOption(const QString &option, JASPControl *host);
	///< mds-plan C10/D2.4+D2.5: HostFilterSelection calls this when its selected filter moved
	///  (after the restamp walk, so consumers re-read a settled namespace). Re-emits
	///  selectionHostFilterChanged, which AnalysisBase uses to refresh its provider view.
	void					notifySelectionHostFilterChanged(JASPControl * host)	{ emit selectionHostFilterChanged(host); }

	///< The raw options JSON of the current syntax-bridge parse request (set per request, empty
	///  clears). A selection host registering after the parse resolves its own option out of
	///  this and writes the resulting filter id into the analysis values before its DropDown
	///  binds - so late hosts see exactly what early hosts got.
	void					setPendingDataSetSelectionRawValues(const Json::Value &rawOptions);

	///< Resolve one dataset-selection syntax surface string to its filter id (exact lookup in
	///< Workspace::datasetSelectionSurfaceMap), "" when nothing matches. The bridge translates
	///< through the very same map; QML tests use this to replay the shown wrapper call.
	Q_INVOKABLE QString		resolveDataSetSelectionText(const QString &surfaceText) const;
	QString					getError(bool withControlName = false);

	bool					isOwnComputedColumn(const std::string& col)			const	{ return _analysis ? _analysis->isOwnComputedColumn(col) : false; }

	bool					needsRefresh()			const;

	QString					info()					const	{ return _info;								}
	QString					infoBottom()			const	{ return _infoBottom;						}
	QString					helpMD()				const;
	QString					metaHelpMD()			const;
	QString					errors()				const	{ return msgsListToString(_formErrors);		}
	QString					warnings()				const	{ return msgsListToString(_formWarnings);	}
	QVariant				analysis()				const	{ return QVariant::fromValue(_analysis);	}
	AnalysisBase*			analysisObj()			const	{ return _analysis;							}
	RSyntax*				rSyntax()				const	{ return _rSyntax;							}
	QString					generateRSyntax(bool useHtml = false) const;
	QVariantList			optionNameConversion()	const;
	bool					isFormulaName(const QString& name)	const;
	bool					isColumnFreeOrMine(const QString & name) const;

	stringvecvec			getValuesFromRSource(const QString& sourceID, const QStringList& searchPath);
	void					addColumnControl(JASPControl* control, bool isComputed);

	const Json::Value	&	boundValues()																		const	{ return _analysis ? _analysis->boundValues() : Json::Value::null; }
	const Json::Value	&	boundValue(const std::string& name, const QVector<JASPControl::ParentKey>& parentKeys)		{ return _analysis ? _analysis->boundValue(name, parentKeys) : Json::Value::null; }
	void					setBoundValue(const std::string& name, const Json::Value& value, const Json::Value& meta, const QVector<JASPControl::ParentKey>& parentKeys = {});
	stringset				usedVariables();

	void					sortControls(QList<JASPControl*>& controls);
	QString					getSyntaxName(const QString& name)				const;
	void					setHasVolatileNotes(bool hasVolatileNotes);
	void					setActiveJASPControl(JASPControl* control, bool hasActiveFocus);
	JASPControl*			getActiveJASPControl()	{ return _activeJASPControl; }

	static const QString	rSyntaxControlName;
		
	GENERIC_SET_FUNCTION(Info					, _info					, infoChanged					, QString		)
	GENERIC_SET_FUNCTION(InfoBottom				, _infoBottom			, infoBottomChanged				, QString		)
	
private:

	Json::Value	&	_getParentBoundValue(const QVector<JASPControl::ParentKey>& parentKeys);
	void			_unregisterDeadControl(JASPControl * deadControl);
	void			_setUpControls();
	void			_setUpModels();
	void			_setUp();
	QString			_getControlLabel(QString controlName);
	void			_addLoadingError(QStringList wrongJson);
	void			setControlIsDependency(	QString controlName, bool isDependency);
	void			setControlMustContain(	QString controlName, QStringList containThis);
	void			setControlIsDependency(	std::string controlName, bool isDependency)					{ setControlIsDependency(tq(controlName), isDependency);	}
	void			setControlMustContain(	std::string controlName, std::set<std::string> containThis)	{ setControlMustContain(tq(controlName), tql(containThis)); }
	void			setAnalysisUp();
	///< mds-plan C10/D2.4: pair the registry/provider signals onto an analysis (both pairing
	///  points - setAnalysis and the syntax/unit-test dummy made in setAnalysisUp - run this,
	///  unique-connected so it is idempotent).
	void			connectSelectionNamespaceSignals(AnalysisBase * analysis);
	stringvecvec	_getValuesFromJson(const Json::Value& jsonValues, const QStringList& searchPath);
	QString			msgsListToString(const QStringList & list) const;
	void			lockOptions();
	void			_disableControls(QQuickItem * root, bool disable);
	Json::Value		_controlOptionMeta(JASPControl* ctrl, bool includeDescriptions) const;

private slots:
	   void			formCompletedHandler();
	   void			knownIssuesUpdated();
	   ///< mds-plan D2.3: unregistering a selection host is eager, not only read-time pruning
	   ///  (wired at registration; entries whose host is dying or dead drop out).
	   void			handleSelectionHostDestroyed(QObject * host);

private:
	AnalysisBase								*	_analysis			= nullptr;
	VariableInfo								*	_varInfo			= nullptr;
	QMap<QString, JASPControl* >					_controls;

	///Ordered on dependencies within QML, aka an assigned variables list depends on the available list it is connected to.
	QVector<JASPControl*>							_dependsOrderedCtrls;
	QMap<QString, ListModel* >						_modelMap;
	QVector<ExpanderButtonBase*>					_expanders;
	QMap<JASPControl*, ExpanderButtonBase*>			_controlExpanderMap;
	bool											_removed 						= false;
	stringset										_mustBe;
	std::map<std::string,stringset>					_mustContain;

	QStringList										_formErrors,
													_formWarnings;
	QQmlComponent*									_controlErrorMessageComponent	= nullptr;
	QList<QQuickItem*>								_controlErrorMessageCache;
	bool											_runOnChange					= true,
													_formCompleted					= false,
													_hasVolatileNotes				= false,
													_initialized					= false,
													_valueChangedEmittedButBlocked	= false;
	QString											_info,
													_infoBottom;
	struct DataSetSelectionRegistration
	{
		QPointer<JASPControl>	host;
		QString					option;
	};
	mutable QVector<DataSetSelectionRegistration>			_dataSetSelectionHosts;			///< mds-plan C4/REGISTRY (dead entries pruned at read time by the const accessor)
	Json::Value										_pendingSelectionRawOptions;		///< raw JSON of the current bridge parse (see setPendingDataSetSelectionRawValues)
	void											_applyPendingDataSetSelectionValue(const QString &option);
	int												_valueChangedSignalsBlocked		= 0;
	std::queue<std::tuple<QString, QString, bool>>	_waitingRScripts; //Sometimes signals are blocked, and thus rscripts. But they shouldnt just disappear right?
	qstringset										_waitingFilters;
	RSyntax										*	_rSyntax						= nullptr;
	bool											_showRButton					= false,
													_developerMode					= false;
	bool											_relaxInputConstraints			= false;
	JASPControl*									_activeJASPControl				= nullptr;
};

#endif // ANALYSISFORM_H
