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

#ifndef VARIABLESLISTBASE_H
#define VARIABLESLISTBASE_H

#include "jasplistcontrol.h"
#include "hostfilterselection.h"
#include <QVariant>
#include <QList>

class ListModelDraggable;
class CheckBoxBase;
class AnalysisForm;

class VariablesListBase : public JASPListControl, public BoundControl
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( ListViewType		listViewType						READ listViewType						WRITE setListViewType						NOTIFY listViewTypeChanged						)
	Q_PROPERTY( int					columns								READ columns							WRITE setColumns							NOTIFY columnsChanged							)
	Q_PROPERTY( QStringList			dropKeys							READ dropKeys							WRITE setDropKeys							NOTIFY dropKeysChanged							)
	Q_PROPERTY( QString				interactionHighOrderCheckBox		READ interactionHighOrderCheckBox		WRITE setInteractionHighOrderCheckBox		NOTIFY interactionHighOrderCheckBoxChanged		)
	Q_PROPERTY( bool				keepVariablesWhenMoved				READ keepVariablesWhenMoved				WRITE setKeepVariablesWhenMoved				NOTIFY keepVariablesWhenMovedChanged			)
	Q_PROPERTY( bool				addInteractionsByDefault			READ addInteractionsByDefault			WRITE setAddInteractionsByDefault			NOTIFY addInteractionsByDefaultChanged			)
	Q_PROPERTY( bool				interactionContainLowerTerms		READ addInteractionsByDefault			WRITE setInteractionContainLowerTerms		NOTIFY interactionContainLowerTermsChanged		)
	Q_PROPERTY( QVariant			sourceWithoutDefaultInteraction		READ sourceWithoutDefaultInteraction	WRITE setSourceWithoutDefaultInteraction	NOTIFY sourceWithoutDefaultInteractionChanged	)
	///< mds-plan C10: dataset/filter selection lives on the LIST (exactly like TextAreaBase's of
	///  the same names): declaring an option name gives this list its own selection, whose value
	///  is a FILTER ID carried by that option (a FilterSelect in the list's chrome reads/writes
	///  it, desktop and syntax alike). Two lists of one form can then look at two datasets at the
	///  same time; a list without one inherits the namespace of the available list its items come
	///  from (or the analysis-wide VariableInfo). It also marks the analysis multiDataSetAware.
	Q_PROPERTY( QString				dataSetSelectionOption				READ dataSetSelectionOption			WRITE setDataSetSelectionOption			NOTIFY dataSetSelectionOptionChanged			)
	Q_PROPERTY( bool				selectionAvailable					READ selectionAvailable													NOTIFY selectionAvailableChanged					)
	Q_PROPERTY( QVariantList		filterSelectionValues				READ filterSelectionValues												NOTIFY filterSelectionValuesChanged				)
	Q_PROPERTY( int					selectedFilterId					READ selectedFilterId			WRITE setSelectedFilterId					NOTIFY selectedFilterIdChanged					)
	///< This list's VariableInfo (provider = selected filter) once it has a selection.
	Q_PROPERTY( VariableInfo *		ownVarInfo							READ ownVarInfo														NOTIFY selectionAvailableChanged				)

public:
	VariablesListBase(QQuickItem* parent = nullptr);
	
	void						setUp()																							override;
	ListModel				*	model()																					const	override;
	ListModelDraggable		*	draggableModel()																		const				{ return _draggableModel;							}
	void						setUpModel()																					override;
	void						bindTo(const Json::Value &value)																override	{ _boundControl->bindTo(value);						}
	const Json::Value		&	boundValue()																			const	override	{ return _boundControl->boundValue();				}
	bool						isJsonValid(const Json::Value& optionValue)												const	override	{ return _boundControl->isJsonValid(optionValue);	}
	void						resetBoundValue()																				override	{ return _boundControl->resetBoundValue();			}
	Json::Value					createJson()																			const	override	{ return _boundControl->createJson();				}
	Json::Value					createEngineMeta()																				const	override	{ return _boundControl->createEngineMeta();				}
	void						setBoundValue(const Json::Value& value, 
											  bool emitChange = true)															override	{ return _boundControl->setBoundValue(value, emitChange);	}
	const Json::Value		&	defaultBoundValue()																		const	override	{ return _boundControl->defaultBoundValue();		}
	void						setDefaultBoundValue(const Json::Value& defaultValue)											override	{ _boundControl->setDefaultBoundValue(defaultValue); }

	ListViewType				listViewType()																			const				{ return _listViewType;								}
	BoundControl			*	boundControl()																					override	{ return _boundControl;								}
	const BoundControl		*	boundControl()																			const	override	{ return _boundControl;								}
	int							columns()																				const				{ return _columns;									}
	const QStringList		&	dropKeys()																				const				{ return _dropKeys;									}
	const QString			&	interactionHighOrderCheckBox()															const				{ return _interactionHighOrderCheckBox;				}
	bool						addRowControl(const TermRowKey& key, JASPControl* control)											override;
	void						moveItems(QList<int> &indexes, ListModelDraggable* dropModel, int dropItemIndex = -1);
	bool						keepVariablesWhenMoved()																const				{ return _keepVariablesWhenMoved;					}
	bool						containsInteractions()																	const	override;
	///< mds-plan D6.1: a list that declares its own dataset/filter selection IS declaring that its
	///  values are columns of that namespace (C10 list-as-host, C13 host-bound) - no available-list
	///  or dataset-source relation required. Without this the second-source pattern (a standalone
	///  AssignedVariablesList with its own dataSetSelectionOption) never encodes: encodeValue()
	///  gates on containsVariables(), and the list's values would stay raw names that no slice
	///  column matches (found by the D6 zoo; zero effect on lists without a selection).
	bool						containsVariables()																				const	override;
	bool						addInteractionsByDefault()																const				{ return _addInteractionsByDefault;					}
	bool						interactionContainLowerTerms()															const				{ return _interactionContainLowerTerms;				}
	QVariant					sourceWithoutDefaultInteraction()														const				{ return _sourceWithoutDefaultInteraction;			}

	// ---- per-list dataset/filter selection (HostFilterSelection; mds-plan C10) ----
	QString					dataSetSelectionOption()			const	{ return _hostSelection.option();			}
	bool					selectionAvailable()				const	{ return _hostSelection.selectionAvailable(); }
	QVariantList			filterSelectionValues()				const	{ return _hostSelection.filterSelectionValues(); }
	int						selectedFilterId()					const	{ return _hostSelection.selectedFilterId(); }
	VariableInfo		  * ownVarInfo()							{ return _hostSelection.ownVarInfo(); }
	VariableInfo		  * ownedSelectionVarInfo() override			{ return _hostSelection.selectionAvailable() ? _hostSelection.ownVarInfo() : nullptr; }
	///< Registry-facing selected filter of this host (mds-plan D2.3).
	Filter				  * selectedSelectionFilter() const override		{ return _hostSelection.selectedFilter(); }
	///< The option the list's FilterSelect dropdown is bound to; its registered control is what
	///< consumers of this host wait for in bindTo order (JASPControl::selectionDependency).
	QString				  ownedSelectionOptionName() const override	{ return _hostSelection.option(); }

	///< A list inherits the namespace of the control its items come from: explicit `source`
	///< (JASPListControl) first, else - when this list has no selection of its own - the related
	///< available list (dropKeys[0], set in _setRelations). A list that declares its own
	///< dataSetSelectionOption serves itself and skips the inherited edge (C13 host-bound).
	///< mds-plan D4.1/D4.2: the direct relation reads behind JASPListControl::resolvedSelectionHost's
	///< chain (the deleted walk's namespace override lived here).
	JASPListControl		  * relatedNamespaceList() override;

signals:
	void listViewTypeChanged();
	void columnsChanged();
	void dropKeysChanged();
	void interactionHighOrderCheckBoxChanged();
	void keepVariablesWhenMovedChanged();
	void addInteractionsByDefaultChanged();
	void interactionContainLowerTermsChanged();
	void sourceWithoutDefaultInteractionChanged();
	void dataSetSelectionOptionChanged();
	void selectionAvailableChanged();
	void filterSelectionValuesChanged();
	void selectedFilterIdChanged();

public slots:
	void setVariableType(int index, int type);
	void setDataSetSelectionOption(const QString & option)	{ _hostSelection.setOption(option); emit dataSetSelectionOptionChanged(); }
	void setSelectedFilterId(int filterId)					{ _hostSelection.setSelectedFilterId(filterId); }

protected slots:
	void termsChangedHandler()		override;
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList, int dropItemIndex);
	void interactionHighOrderHandler(JASPControl* checkBoxControl);
	void handleFormIsKnown(AnalysisForm * form);
	void handleAnalysisChanged();
	void handleAnalysisFilterChanged()	{ _hostSelection.analysisFilterMaybeChanged(); }

protected:
	GENERIC_SET_FUNCTION(ListViewType,						_listViewType,						listViewTypeChanged,					ListViewType	)
	GENERIC_SET_FUNCTION(Columns,							_columns,							columnsChanged,							int				)
	GENERIC_SET_FUNCTION(InteractionHighOrderCheckBox,		_interactionHighOrderCheckBox,		interactionHighOrderCheckBoxChanged,	QString			)
	GENERIC_SET_FUNCTION(KeepVariablesWhenMoved,			_keepVariablesWhenMoved,			keepVariablesWhenMovedChanged,			bool			)
	GENERIC_SET_FUNCTION(AddInteractionsByDefault,			_addInteractionsByDefault,			addInteractionsByDefaultChanged,		bool			)
	GENERIC_SET_FUNCTION(InteractionContainLowerTerms,		_interactionContainLowerTerms,		interactionContainLowerTermsChanged,	bool			)
	GENERIC_SET_FUNCTION(SourceWithoutDefaultInteraction,	_sourceWithoutDefaultInteraction,	sourceWithoutDefaultInteractionChanged,	QVariant		)

	void						_setInitialized(const Json::Value& value = Json::nullValue)	override;
	void						setDropKeys(const QStringList& dropKeys);
	ListModel*					getRelatedModel();

private:
	void						_setRelations();
	
protected:
	ListModelDraggable*			_draggableModel					= nullptr;
	ListViewType				_listViewType					= ListViewType::AssignedVariables;
	BoundControl*				_boundControl					= nullptr;
	bool						_keepVariablesWhenMoved			= false,
								_addInteractionsByDefault		= true,
								_interactionContainLowerTerms	= true;

	int							_columns				= 1;

	ListModelDraggable	*		_tempDropModel = nullptr;
	QList<int>					_tempIndexes;
	int							_tempDropItemIndex;

	QStringList					_dropKeys;
	QVariant					_sourceWithoutDefaultInteraction;
	QString						_interactionHighOrderCheckBox;

	HostFilterSelection			_hostSelection			{ this };

	
};

#endif // VARIABLESLISTBASE_H
