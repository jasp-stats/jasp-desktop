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
#include "analysis/jaspcontrol.h"
#include "listmodeldraggable.h"
#include "analysis/options/boundcontrol.h"


class VariablesListBase : public JASPListControl, public BoundControl
{
	Q_OBJECT

	Q_PROPERTY( ListViewType	listViewType	READ listViewType		WRITE setListViewType	NOTIFY listViewTypeChanged	)
	Q_PROPERTY( int				columns			READ columns			WRITE setColumns		NOTIFY columnsChanged		)
public:
	VariablesListBase(QQuickItem* parent = nullptr);
	
	void						setUp()							override;
	ListModel*					model()					const	override	{ return _draggableModel;	}
	ListModelDraggable*			draggableNodel()		const				{ return _draggableModel;	}
	void						setUpModel()					override;
	void						bindTo(Option *option)			override	{ _boundControl->bindTo(option);				}
	void						unbind()						override	{ _boundControl->unbind();						}
	Option*						createOption()					override	{ return _boundControl->createOption();			}
	Option*						boundTo()						override	{ return _boundControl->boundTo();				}
	bool						isOptionValid(Option* option)	override	{ return _boundControl->isOptionValid(option);	}
	bool						isJsonValid(const Json::Value& optionValue) override { return _boundControl->isJsonValid(optionValue);	}

	ListViewType				listViewType()			const				{ return _listViewType;		}
	BoundControl*				boundControl()								{ return _boundControl;		}
	int							columns()				const				{ return _columns;			}
	
	void						moveItems(QList<int> &indexes, ListModelDraggable* dropModel, int dropItemIndex = -1, JASPControl::AssignType assignOption = JASPControl::AssignType::AssignDefault);

signals:
	void listViewTypeChanged();
	void columnsChanged();

protected:
	GENERIC_SET_FUNCTION(ListViewType,	_listViewType,	listViewTypeChanged,	ListViewType	)
	GENERIC_SET_FUNCTION(Columns,		_columns,		columnsChanged,			int				)

	ListModelDraggable*			_draggableModel	= nullptr;
	ListViewType				_listViewType	= ListViewType::AssignedVariables;
	BoundControl*				_boundControl	= nullptr;

protected slots:
	void termsChangedHandler()		override;
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList, int dropItemIndex, int assignOption);
	
private:
	int							_columns				= 1;

	ListModelDraggable	*		_tempDropModel = nullptr;
	QList<int>					_tempIndexes;
	int							_tempDropItemIndex;
	JASPControl::AssignType		_tempAssignOption = JASPControl::AssignType::AssignDefault;
	
};

#endif // VARIABLESLISTBASE_H
