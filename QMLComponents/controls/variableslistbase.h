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
#include <QVariant>
#include <QList>

class ListModelDraggable;
class CheckBoxBase;
class ColumnTypesModel;

class VariablesListBase : public JASPListControl, public BoundControl
{
	Q_OBJECT

	Q_PROPERTY( ListViewType		listViewType					READ listViewType					WRITE setListViewType					NOTIFY listViewTypeChanged					)
	Q_PROPERTY( int					columns							READ columns						WRITE setColumns						NOTIFY columnsChanged						)
	Q_PROPERTY( QStringList			suggestedColumns				READ allowedColumns					WRITE setAllowedColumns					NOTIFY allowedColumnsChanged				)
	Q_PROPERTY( QStringList			allowedColumns					READ allowedColumns					WRITE setAllowedColumns					NOTIFY allowedColumnsChanged				)
	Q_PROPERTY(	QStringList			allowedColumnsIcons				READ allowedColumnsIcons													NOTIFY allowedColumnsIconsChanged			)
	Q_PROPERTY( QStringList			columnsTypes					READ columnsTypes															NOTIFY columnsTypesChanged					)
	Q_PROPERTY( QStringList			columnsNames					READ columnsNames															NOTIFY columnsNamesChanged					)
	Q_PROPERTY( QStringList			dropKeys						READ dropKeys						WRITE setDropKeys						NOTIFY dropKeysChanged						)
	Q_PROPERTY( QString				interactionHighOrderCheckBox	READ interactionHighOrderCheckBox	WRITE setInteractionHighOrderCheckBox	NOTIFY interactionHighOrderCheckBoxChanged	)
	Q_PROPERTY( QAbstractListModel* allowedTypesModel				READ allowedTypesModel														NOTIFY allowedTypesModelChanged				)
	Q_PROPERTY( int					minNumericLevels				READ minNumericLevels				WRITE setMinNumericLevels				NOTIFY minNumericLevelsChanged				)
	Q_PROPERTY( int					maxNumericLevels				READ maxNumericLevels				WRITE setMaxNumericLevels				NOTIFY maxNumericLevelsChanged				)
	Q_PROPERTY( int					minLevels						READ minLevels						WRITE setMinLevels						NOTIFY minLevelsChanged						)
	Q_PROPERTY( int					maxLevels						READ maxLevels						WRITE setMaxLevels						NOTIFY maxLevelsChanged						)
	Q_PROPERTY( bool				allowTypeChange					READ allowTypeChange				WRITE setAllowTypeChange				NOTIFY allowTypeChangeChanged				)

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
	Json::Value					createMeta()																			const	override	{ return _boundControl->createMeta();				}
	void						setBoundValue(const Json::Value& value, 
											  bool emitChange = true)															override	{ return _boundControl->setBoundValue(value, emitChange);	}
	const Json::Value		&	defaultBoundValue()																		const	override	{ return _boundControl->defaultBoundValue();		}
	void						setDefaultBoundValue(const Json::Value& defaultValue)											override	{ _boundControl->setDefaultBoundValue(defaultValue); }

	ListViewType				listViewType()																			const				{ return _listViewType;								}
	BoundControl			*	boundControl()																					override	{ return _boundControl;								}
	int							columns()																				const				{ return _columns;									}
	const QStringList		&	allowedColumns()																		const				{ return _allowedColumns;							}
	QStringList					allowedColumnsIcons()																	const;
	const QStringList		&	columnsTypes()																			const				{ return _columnsTypes;								}
	const QStringList		&	columnsNames()																			const				{ return _columnsNames;								}
	const QStringList		&	dropKeys()																				const				{ return _dropKeys;									}
	const QString			&	interactionHighOrderCheckBox()															const				{ return _interactionHighOrderCheckBox;				}
	bool						addRowControl(const QString& key, JASPControl* control)											override;
	void						moveItems(QList<int> &indexes, ListModelDraggable* dropModel, int dropItemIndex = -1);
	QAbstractListModel		*	allowedTypesModel();
	bool						isTypeAllowed(columnType type)															const	override;
	columnType					defaultType()																			const	override;
	int							minLevels()																				const				{ return _minLevels;			}
	int							maxLevels()																				const				{ return _maxLevels;			}
	int							minNumericLevels()																		const				{ return _minNumericLevels;		}
	int							maxNumericLevels()																		const				{ return _maxNumericLevels;		}
	bool						allowTypeChange()																		const				{ return _allowTypeChange;		}

signals:
	void listViewTypeChanged();
	void columnsChanged();
	void allowedColumnsChanged();
	void allowedColumnsIconsChanged();
	void columnsTypesChanged();
	void columnsNamesChanged();
	void dropKeysChanged();
	void interactionHighOrderCheckBoxChanged();
	void allowedTypesModelChanged();
	void minLevelsChanged();
	void maxLevelsChanged();
	void minNumericLevelsChanged();
	void maxNumericLevelsChanged();
	void allowTypeChangeChanged();

public slots:
	void setVariableType(int index, int type);

protected slots:
	void termsChangedHandler()		override;
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList, int dropItemIndex);
	void interactionHighOrderHandler(JASPControl* checkBoxControl);

protected:
	GENERIC_SET_FUNCTION(ListViewType,					_listViewType,					listViewTypeChanged,					ListViewType	)
	GENERIC_SET_FUNCTION(Columns,						_columns,						columnsChanged,							int				)
	GENERIC_SET_FUNCTION(AllowedColumns,				_allowedColumns,				allowedColumnsChanged,					QStringList		)
	GENERIC_SET_FUNCTION(ColumnsTypes,					_columnsTypes,					columnsTypesChanged,					QStringList		)
	GENERIC_SET_FUNCTION(ColumnsNames,					_columnsNames,					columnsNamesChanged,					QStringList		)
	GENERIC_SET_FUNCTION(InteractionHighOrderCheckBox,	_interactionHighOrderCheckBox,	interactionHighOrderCheckBoxChanged,	QString			)
	GENERIC_SET_FUNCTION(MinLevels,						_minLevels,						minLevelsChanged,						int				)
	GENERIC_SET_FUNCTION(MaxLevels,						_maxLevels,						maxLevelsChanged,						int				)
	GENERIC_SET_FUNCTION(MinNumericLevels,				_minNumericLevels,				minNumericLevelsChanged,				int				)
	GENERIC_SET_FUNCTION(MaxNumericLevels,				_maxNumericLevels,				maxNumericLevelsChanged,				int				)
	GENERIC_SET_FUNCTION(AllowTypeChange,				_allowTypeChange,				allowTypeChangeChanged,					bool			)

	void						_setInitialized(const Json::Value& value = Json::nullValue)	override;
	void						setDropKeys(const QStringList& dropKeys);
	ListModel*					getRelatedModel();

private:
	void						_setAllowedVariables();
	void						_setRelations();
	
protected:
	ListModelDraggable*			_draggableModel	= nullptr;
	ListViewType				_listViewType	= ListViewType::AssignedVariables;
	BoundControl*				_boundControl	= nullptr;
	
private:
	int							_columns				= 1;

	ListModelDraggable	*		_tempDropModel = nullptr;
	QList<int>					_tempIndexes;
	int							_tempDropItemIndex,
								_minNumericLevels =		-1,
								_maxNumericLevels =		-1,
								_minLevels =			-1,
								_maxLevels =			-1;

	bool						_allowTypeChange =		true;
	QStringList					_allowedColumns,
								_columnsTypes,
								_columnsNames,
								_dropKeys;
	QString						_interactionHighOrderCheckBox;

	ColumnTypesModel *			_allowedTypesModel = nullptr;
	
};

#endif // VARIABLESLISTBASE_H
