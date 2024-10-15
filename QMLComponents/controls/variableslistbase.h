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

	Q_PROPERTY( ListViewType		listViewType					READ listViewType					WRITE setListViewType					NOTIFY listViewTypeChanged					)
	Q_PROPERTY( int					columns							READ columns						WRITE setColumns						NOTIFY columnsChanged						)
	Q_PROPERTY( QStringList			dropKeys						READ dropKeys						WRITE setDropKeys						NOTIFY dropKeysChanged						)
	Q_PROPERTY( QString				interactionHighOrderCheckBox	READ interactionHighOrderCheckBox	WRITE setInteractionHighOrderCheckBox	NOTIFY interactionHighOrderCheckBoxChanged	)

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
	const QStringList		&	dropKeys()																				const				{ return _dropKeys;									}
	const QString			&	interactionHighOrderCheckBox()															const				{ return _interactionHighOrderCheckBox;				}
	bool						addRowControl(const QString& key, JASPControl* control)											override;
	void						moveItems(QList<int> &indexes, ListModelDraggable* dropModel, int dropItemIndex = -1);

signals:
	void listViewTypeChanged();
	void columnsChanged();
	void dropKeysChanged();
	void interactionHighOrderCheckBoxChanged();

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
	GENERIC_SET_FUNCTION(InteractionHighOrderCheckBox,	_interactionHighOrderCheckBox,	interactionHighOrderCheckBoxChanged,	QString			)

	void						_setInitialized(const Json::Value& value = Json::nullValue)	override;
	void						setDropKeys(const QStringList& dropKeys);
	ListModel*					getRelatedModel();

private:
	void						_setRelations();
	
protected:
	ListModelDraggable*			_draggableModel	= nullptr;
	ListViewType				_listViewType	= ListViewType::AssignedVariables;
	BoundControl*				_boundControl	= nullptr;
	
private:
	int							_columns				= 1;

	ListModelDraggable	*		_tempDropModel = nullptr;
	QList<int>					_tempIndexes;
	int							_tempDropItemIndex;

	QStringList					_dropKeys;
	QString						_interactionHighOrderCheckBox;

	
};

#endif // VARIABLESLISTBASE_H
