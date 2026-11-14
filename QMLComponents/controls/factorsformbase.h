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

#ifndef FACTORSFORMBASE_H
#define FACTORSFORMBASE_H

#include "jasplistcontrol.h"
#include "boundcontrols/boundcontrolbase.h"
#include "models/listmodelfactorsform.h"

class VariablesListBase;

class FactorsFormBase :  public JASPListControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( int			initNumberFactors		READ initNumberFactors		WRITE setInitNumberFactors	NOTIFY initNumberFactorsChanged	)
	Q_PROPERTY( int			countVariables			READ countVariables										NOTIFY countVariablesChanged	)
	Q_PROPERTY(QString		baseName				READ baseName				WRITE setBaseName			NOTIFY baseNameChanged			)
	Q_PROPERTY(QString		baseTitle				READ baseTitle				WRITE setBaseTitle			NOTIFY baseTitleChanged			)
	Q_PROPERTY(int			startIndex				READ startIndex				WRITE setStartIndex			NOTIFY startIndexChanged		)
	Q_PROPERTY(bool			nested					READ nested					WRITE setNested				NOTIFY nestedChanged			)
	Q_PROPERTY(bool			allowInteraction		READ allowInteraction		WRITE setAllowInteraction	NOTIFY allowInteractionChanged	)
	Q_PROPERTY(QVariantList	factorsTitles			READ factorsTitles										NOTIFY factorsTitlesChanged		)
	Q_PROPERTY(QVariantList	factorsItems			READ factorsItems										NOTIFY factorsItemsChanged		)

	// Term specification (factorsformbase_specifications.cpp): opt-in per analysis, the option does not change while
	// advancedAvailable is false. `advanced` switches it on (the "Specify" button; `advanced: true` in QML starts new
	// analyses with it).
	Q_PROPERTY(bool			advancedAvailable			READ advancedAvailable			WRITE setAdvancedAvailable			NOTIFY advancedAvailableChanged			)
	Q_PROPERTY(bool			advanced					READ advanced					WRITE setAdvanced					NOTIFY advancedChanged					)
	Q_PROPERTY(bool			advancedActive				READ advancedActive												NOTIFY advancedActiveChanged			)
	Q_PROPERTY(bool			restrictionAvailable		READ restrictionAvailable		WRITE setRestrictionAvailable		NOTIFY restrictionAvailableChanged		)
	Q_PROPERTY(QStringList	continuousSpecifications	READ continuousSpecifications	WRITE setContinuousSpecifications	NOTIFY continuousSpecificationsChanged	)
	Q_PROPERTY(QStringList	categoricalSpecifications	READ categoricalSpecifications	WRITE setCategoricalSpecifications	NOTIFY categoricalSpecificationsChanged	)
	Q_PROPERTY(QString		interceptMode				READ interceptMode				WRITE setInterceptMode				NOTIFY interceptModeChanged				)	///< "optional" ((Intercept) is a term), "forced" (always in) or "none" (no intercept)
	Q_PROPERTY(int			specificationsRevision		READ specificationsRevision										NOTIFY specificationsChanged			)
	Q_PROPERTY(int			modelsRevision				READ modelsRevision												NOTIFY modelsChanged					)	///< changes with the terms of any model
	Q_PROPERTY(QStringList	definitionTerms				READ definitionTerms											NOTIFY definitionTermsChanged			)	///< the terms the available list offers while the specification is on

public:
	FactorsFormBase(QQuickItem* parent = nullptr);

	bool			isJsonValid(const Json::Value& optionValue)	const	override;
	Json::Value		createJson()								const	override;
	void			bindTo(const Json::Value& value)					override;
	ListModel*		model()										const	override	{ return _factorsModel; }
	void			setUpModel()										override;
	void			setUp()												override;
	bool			containsVariables()							const	override	{ return true; };

	// (per-factor provenance): the transient `.filterIds` sidecar node of this
	//option - an ARRAY-OF-ARRAYS, element i being factor i's per-term positional filterId array
	//(the schema's positional grammar one level up: factors are POSITIONAL in the bound
	//value, so no named-children grammar and no factor-name stability problem). Null when
	//nothing is stamped (non-aware/hostless runs generate no node, byte-identical trees).
	//While the term specification is on, element i is an object instead: the positional ids
	//of factor i's `components` and `definitions` members.
	Json::Value		createFilterIds()							const	override;

	///< The rows of the "Specify" table (FactorsFormDefinitions.qml): the intercept (when it has one), then every
	///  definition, each a map with id, label, variable, specification, parameters, restriction and what the editors need.
	Q_INVOKABLE QVariantList	definitionRows()																const;
	///< key: "variable" (a variable, or the constant-offset choice), "value" (of a constant offset), "specification",
	///  "restriction" or a parameter name. Refused (with a warning) when two definitions would become the same or give the
	///  same label; a changed definition keeps its place in every model (renamed there).
	Q_INVOKABLE void			setDefinitionValue(QString id, QString key, QString value);
	Q_INVOKABLE void			addDefinition();				///< an empty row at the bottom
	Q_INVOKABLE void			removeDefinition(QString id);	///< not a base definition
	Q_INVOKABLE void			resetDefinitions();				///< "Reset": specification off, the table back to the base definitions
	Terms						initialModelTerms()																const;	///< a new model's terms: its intercept while the specification is on
	///< The terms of one model for its full-width view (FactorsFormModelView.qml): labels, interactions joined with a star.
	Q_INVOKABLE QVariantList	modelTermRows(int factorIndex)													const;
	Q_INVOKABLE void			removeModelTerm(int factorIndex, int termIndex);	///< removes one term from one model

	Q_INVOKABLE	void	addFactor()													{ _factorsModel->addFactor();						}
	Q_INVOKABLE void	removeFactor()												{ _factorsModel->removeFactor();					}
	Q_INVOKABLE void	titleChanged(int index, QString title)						{ _factorsModel->titleChangedSlot(index, title);	}
	Q_INVOKABLE void	factorAdded(int index, QVariant item);

	int					initNumberFactors()						const				{ return _initNumberFactors;						}
	int					countVariables()						const				{ return initialized() ? _factorsModel->countVariables() : 0; }
	JASPListControl*	availableVariablesList()				const				{ return _availableVariablesListItem;				}
	QString				baseName()								const				{ return _baseName;			}
	QString				baseTitle()								const				{ return _baseTitle;		}
	int					startIndex()							const				{ return _startIndex;		}
	bool				nested()								const				{ return _nested;			}
	bool				allowInteraction()						const				{ return _allowInteraction;	}
	QVariantList		factorsTitles()							const;
	QVariantList		factorsItems()							const;
	bool				advancedAvailable()						const				{ return _advancedAvailable;			}
	bool				advanced()								const				{ return _advanced;						}
	bool				advancedActive()						const				{ return _advancedAvailable && _advanced;	}
	bool				nestedActive()							const				{ return _nested && !advancedActive();	}	///< with the term specification on, models are not forced nested
	bool				restrictionAvailable()					const				{ return _restrictionAvailable;			}
	QStringList			continuousSpecifications()				const				{ return _continuousSpecifications;		}
	QStringList			categoricalSpecifications()				const				{ return _categoricalSpecifications;		}
	QString				interceptMode()							const				{ return _interceptMode;					}
	int					specificationsRevision()				const				{ return _specificationsRevision;		}
	QStringList			definitionTerms()						const;
	int					modelsRevision()						const				{ return _modelsRevision;				}

	void				setAdvancedAvailable(bool available);
	void				setAdvanced(bool advanced);
	GENERIC_SET_FUNCTION(RestrictionAvailable,		_restrictionAvailable,		restrictionAvailableChanged,		bool		)
	GENERIC_SET_FUNCTION(ContinuousSpecifications,	_continuousSpecifications,	continuousSpecificationsChanged,	QStringList	)
	GENERIC_SET_FUNCTION(CategoricalSpecifications,	_categoricalSpecifications,	categoricalSpecificationsChanged,	QStringList	)
	GENERIC_SET_FUNCTION(InterceptMode,				_interceptMode,				interceptModeChanged,				QString		)

	GENERIC_SET_FUNCTION(BaseName			, _baseName			, baseNameChanged			, QString		)
	GENERIC_SET_FUNCTION(BaseTitle			, _baseTitle		, baseTitleChanged			, QString		)
	GENERIC_SET_FUNCTION(StartIndex			, _startIndex		, startIndexChanged			, int			)
	GENERIC_SET_FUNCTION(Nested				, _nested			, nestedChanged				, bool			)
	GENERIC_SET_FUNCTION(AllowInteraction	, _allowInteraction	, allowInteractionChanged	, bool			)

signals:
	void initNumberFactorsChanged();
	void countVariablesChanged();
	void baseNameChanged();
	void baseTitleChanged();
	void startIndexChanged();
	void nestedChanged();
	void allowInteractionChanged();
	void factorsTitlesChanged();
	void factorsItemsChanged();
	void advancedAvailableChanged();
	void advancedChanged();
	void advancedActiveChanged();
	void advancedToggled();			///< the term specification was switched (not a rebind): models are nested again when it goes off
	void restrictionAvailableChanged();
	void continuousSpecificationsChanged();
	void categoricalSpecificationsChanged();
	void interceptModeChanged();
	void specificationsChanged();
	void modelsChanged();
	void definitionTermsChanged();

protected slots:
	void			termsChangedHandler() override;

protected:
	///< live factors-model option value; termsChangedHandler writes exactly this and
	/// the engine generation reads the same builder (createJson stays the defaults builder).
	Json::Value		createEngineValue()												const	override;

	///< (per-factor provenance, the per-term discipline): the Storage file shape.
	/// Each factor's terms serialize as the established per-term objects ({name, filterId?, types},
	/// interactions nested under the optionKey) via Terms::getStorageOptions, so a saved file
	/// keeps every factor term's provenance; the applier (bindTo) parses both this and the
	/// legacy/engine {value, types} shape byte-identically.
	Json::Value		createStorageValue()											const	override;

	///< (the self-rebind landmine, factors flavour): the Engine builder carries
	/// no per-term filterIds, so the null-path self-rebind must round-trip the Storage
	/// shape - same rule and rationale as BoundControlTerms::selfRebindFormat.
	BoundControl::JsonFormat	selfRebindFormat()										const	override	{ return BoundControl::Storage; }

	///< With the term specification on, only the variable references of the option are column names: each
	/// model's optionKey member (`components`) and each definition's `variable` (never titles, ids, labels or levels).
	/// Otherwise the plain {shouldEncode} of every factors form.
	Json::Value					createEngineMeta()										const	override;

	GENERIC_SET_FUNCTION(InitNumberFactors,	_initNumberFactors,		initNumberFactorsChanged,	int	)

private:
	using Factor = ListModelFactorsForm::Factor;

	///< A term of the term specification, defined once for all models: a variable with a specification (transformation or
	///  coding), or a constant offset. Its label (e.g. "log(x)", "offset(2)") is the term the available list offers and
	///  the models hold.
	struct TermDefinition
	{
		QString					id;				///< "d1", "d2", ...: stable, never reused
		QString					variable;		///< empty for a constant offset, and for an added row until a variable is chosen
		QString					specification;	///< "none", "log", ..., "offset", "treatment", ..., or "offsetConstant"
		QMap<QString, QString>	parameters;		///< all parameters ever set (also saved), so switching back restores them; a constant offset's number is "value"
		QString					restriction;	///< a fixed coefficient (restrictionAvailable); never for an offset
	};

	// term specification (factorsformbase_specifications.cpp)
	std::vector<std::pair<QString, columnType>>	_universe()																	const;	///< the analysis' variables and their types
	bool							_universeKnown()																				const;
	columnType						_typeOf(const QString & variable)																const;
	int								_filterIdOf(const QString & variable)															const;
	QString							_label(const TermDefinition & definition)														const;
	const TermDefinition		*	_definitionWithLabel(const QString & label)														const;
	bool							_isBase(const TermDefinition & definition)														const;
	bool							_isComplete(const TermDefinition & definition)													const;
	QMap<QString, QString>			_completeLabels()																				const;	///< id -> label of every complete definition
	void							_removeInvalidInteractions();																		///< no interaction with the intercept or an offset
	QString							_defaultSpecification(columnType type)															const;
	void							_setDefaultParameters(TermDefinition & definition)											const;
	QString							_signature(const TermDefinition & definition)												const;
	QStringList						_allowedSpecifications(columnType type)															const;
	bool							_constantOffsetAllowed()																		const;
	QStringList						_levels(const QString & variable)																const;
	TermDefinition					_newDefinition(const QString & variable);
	void							_syncDefinitions();																					///< definitions follow the analysis' variables
	QStringList						_resolveLabelClashes();
	void							_removeTermsWith(const QStringList & labels);
	void							_definitionsEdited();																				///< renames changed labels in every model, rebuilds the option
	void							_variablesRenamed(const QMap<QString, QString> & renamed);
	void							_levelsRenamed(const QString & variable, const QMap<QString, QString> & renamed);
	void							_switchModels(bool toAdvanced);																		///< basic variables <-> defined terms in every model
	Terms							_basicTerms(const Terms & advancedTerms)														const;
	Terms							_advancedTerms(const Terms & basicTerms)														const;
	void							_connectUniverse();																					///< the definitions follow the analysis' variables (and their renames)
	void							_advancedEngineMembers(const Factor & factor, Json::Value & factorJson, Json::Value * filterIds = nullptr, int hostId = -1)	const;
	Json::Value						_definitionsJson(bool engine, const std::vector<const TermDefinition *> & definitions, Json::Value * types = nullptr)		const;
	static TermDefinition			_definitionFromJson(const Json::Value & row);
	bool							_advancedJsonValid(const Json::Value & value)													const;
	void							_bindAdvanced(Json::Value & value, ListModelFactorsForm::FactorVec & factors);

	std::vector<TermDefinition>		_definitions;
	int								_nextDefinitionId			= 1;
	QString							_interceptRestriction;
	QMap<QString, QString>			_labels;					///< id -> label the models hold, to rename them when a label changes
	JASPListControl				*	_universeList				= nullptr;	///< hidden list of the analysis' variables (the available list shows the defined terms meanwhile)
	bool							_universeConnected			= false;
	int								_modelsRevision				= 0;
	QStringList						_knownVariables;			///< the analysis' variables at the last sync (a new one goes into the last model)

	ListModelFactorsForm*	_factorsModel				= nullptr;
	JASPListControl*		_availableVariablesListItem	= nullptr;
	int						_initNumberFactors			= 1,
							_startIndex					= 1;
	QString					_availableVariablesListName,
							_baseName					= "Factor",   // dont translate it, see: https://github.com/jasp-stats/jasp-issues/issues/2947
							_baseTitle					= tr("Factor");
	bool					_nested						= false,
							_allowInteraction			= false,
							_advancedAvailable			= false,
							_advanced					= false,
							_restrictionAvailable		= false;
	QStringList				_continuousSpecifications	= { "default" },	///< "default": the ones the catalogue offers by default
							_categoricalSpecifications	= { "default" };
	QString					_interceptMode				= "optional";
	int						_specificationsRevision		= 0;

};

#endif // FACTORSFORMBASE_H
