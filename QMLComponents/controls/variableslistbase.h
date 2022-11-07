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

class VariablesListBase : public JASPListControl, public BoundControl
{
	Q_OBJECT

	Q_PROPERTY( ListViewType	listViewType					READ listViewType					WRITE setListViewType					NOTIFY listViewTypeChanged					)
	Q_PROPERTY( int				columns							READ columns						WRITE setColumns						NOTIFY columnsChanged						)
	Q_PROPERTY( QStringList		allowedColumns					READ allowedColumns					WRITE setAllowedColumns					NOTIFY allowedColumnsChanged				)
	Q_PROPERTY( QStringList		suggestedColumns				READ suggestedColumns				WRITE setSuggestedColumns				NOTIFY suggestedColumnsChanged				)
	Q_PROPERTY(	QStringList		suggestedColumnsIcons			READ suggestedColumnsIcons													NOTIFY suggestedColumnsIconsChanged			)
	Q_PROPERTY( QStringList		columnsTypes					READ columnsTypes															NOTIFY columnsTypesChanged					)
	Q_PROPERTY( QStringList		columnsNames					READ columnsNames															NOTIFY columnsNamesChanged					)
	Q_PROPERTY( QStringList		dropKeys						READ dropKeys						WRITE setDropKeys						NOTIFY dropKeysChanged						)
	Q_PROPERTY( QString			interactionHighOrderCheckBox	READ interactionHighOrderCheckBox	WRITE setInteractionHighOrderCheckBox	NOTIFY interactionHighOrderCheckBoxChanged	)

public:
	VariablesListBase(QQuickItem* parent = nullptr);
	
	void						setUp()												override;
	ListModel*					model()										const	override;
	ListModelDraggable*			draggableModel()							const				{ return _draggableModel;							}
	void						setUpModel()										override;
	void						bindTo(const Json::Value &value)					override	{ _boundControl->bindTo(value);						}
	const Json::Value&			boundValue()								const	override	{ return _boundControl->boundValue();				}
	bool						isJsonValid(const Json::Value& optionValue) const	override	{ return _boundControl->isJsonValid(optionValue);	}
	void						resetBoundValue()									override	{ return _boundControl->resetBoundValue();			}
	Json::Value					createJson()								const	override	{ return _boundControl->createJson();				}
	Json::Value					createMeta()								const	override	{ return _boundControl->createMeta();				}
	void						setBoundValue(const Json::Value& value, 
											  bool emitChange = true)				override	{ return _boundControl->setBoundValue(value, emitChange);	}
	const Json::Value&			defaultBoundValue()							const	override	{ return _boundControl->defaultBoundValue();		}
	void						setDefaultBoundValue(const Json::Value& defaultValue) override	{ _boundControl->setDefaultBoundValue(defaultValue); }

	ListViewType				listViewType()								const				{ return _listViewType;								}
	BoundControl*				boundControl()										override	{ return _boundControl;								}
	int							columns()									const				{ return _columns;									}
	const QStringList&			allowedColumns()							const				{ return _allowedColumns;							}
	const QStringList&			suggestedColumns()							const				{ return _suggestedColumns;							}
	const QStringList&			suggestedColumnsIcons()						const				{ return _suggestedColumnsIcons;					}
	const QStringList&			columnsTypes()								const				{ return _columnsTypes;								}
	const QStringList&			columnsNames()								const				{ return _columnsNames;								}
	const QStringList&			dropKeys()									const				{ return _dropKeys;									}
	const QString&				interactionHighOrderCheckBox()				const				{ return _interactionHighOrderCheckBox;				}
	bool						addRowControl(const QString& key, JASPControl* control) override;
	void						moveItems(QList<int> &indexes, ListModelDraggable* dropModel, int dropItemIndex = -1);

signals:
	void listViewTypeChanged();
	void columnsChanged();
	void allowedColumnsChanged();
	void suggestedColumnsChanged();
	void suggestedColumnsIconsChanged();
	void columnsTypesChanged();
	void columnsNamesChanged();
	void dropKeysChanged();
	void interactionHighOrderCheckBoxChanged();

protected:
	GENERIC_SET_FUNCTION(ListViewType,					_listViewType,					listViewTypeChanged,					ListViewType	)
	GENERIC_SET_FUNCTION(Columns,						_columns,						columnsChanged,							int				)
	GENERIC_SET_FUNCTION(AllowedColumns,				_allowedColumns,				allowedColumnsChanged,					QStringList		)
	GENERIC_SET_FUNCTION(SuggestedColumns,				_suggestedColumns,				suggestedColumnsChanged,				QStringList		)
	GENERIC_SET_FUNCTION(SuggestedColumnsIcons,			_suggestedColumnsIcons,			suggestedColumnsIconsChanged,			QStringList		)
	GENERIC_SET_FUNCTION(ColumnsTypes,					_columnsTypes,					columnsTypesChanged,					QStringList		)
	GENERIC_SET_FUNCTION(ColumnsNames,					_columnsNames,					columnsNamesChanged,					QStringList		)
	GENERIC_SET_FUNCTION(InteractionHighOrderCheckBox,	_interactionHighOrderCheckBox,	interactionHighOrderCheckBoxChanged,	QString			)

	void						setDropKeys(const QStringList& dropKeys);

	ListModel*					getRelatedModel();

	ListModelDraggable*			_draggableModel	= nullptr;
	ListViewType				_listViewType	= ListViewType::AssignedVariables;
	BoundControl*				_boundControl	= nullptr;

protected slots:
	void termsChangedHandler()		override;
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList, int dropItemIndex);
	void interactionHighOrderHandler(JASPControl* checkBoxControl);

private:
	int							_getColumnsTypes(const QStringList& types);
	void						_setAllowedVariables();
	void						_setRelations();

	int							_columns				= 1;

	ListModelDraggable	*		_tempDropModel = nullptr;
	QList<int>					_tempIndexes;
	int							_tempDropItemIndex;

	QStringList					_allowedColumns,
								_suggestedColumns,
								_suggestedColumnsIcons,
								_columnsTypes,
								_columnsNames,
								_dropKeys;
	QString						_interactionHighOrderCheckBox;
	
};

#endif // VARIABLESLISTBASE_H
