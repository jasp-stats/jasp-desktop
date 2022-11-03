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

#ifndef COMPONENTSLIST_H
#define COMPONENTSLIST_H

#include "jasplistcontrol.h"
#include "boundcontrols/boundcontrolbase.h"
#include "models/listmodeltermsassigned.h"

class ComponentsListBase : public JASPListControl, public BoundControlBase
{
	Q_OBJECT

	Q_PROPERTY( bool			addItemManually		READ addItemManually		WRITE setAddItemManually		NOTIFY addItemManuallyChanged		)
	Q_PROPERTY( int				minimumItems		READ minimumItems			WRITE setMinimumItems			NOTIFY minimumItemsChanged			)
	Q_PROPERTY( int				maximumItems		READ maximumItems			WRITE setMaximumItems			NOTIFY maximumItemsChanged			)
	Q_PROPERTY( QString			newItemName			READ newItemName			WRITE setNewItemName			NOTIFY newItemNameChanged			)
	Q_PROPERTY( QList<QVariant>	defaultValues		READ defaultValues			WRITE setDefaultValues			NOTIFY defaultValuesChanged			)
	Q_PROPERTY( bool			duplicateWhenAdding	READ duplicateWhenAdding	WRITE setDuplicateWhenAdding	NOTIFY duplicateWhenAddingChanged	)

public:
	ComponentsListBase(QQuickItem* item = nullptr);

	bool			isJsonValid(const Json::Value& optionValue)	const	override;
	Json::Value		createJson()								const	override;
	void			bindTo(const Json::Value& value)					override;
	ListModel*		model()										const	override { return _termsModel; }
	void			setUpModel()										override;

	QString			newItemName()						const			{ return _newItemName;			}
	bool			addItemManually()					const			{ return _addItemManually;		}
	int				minimumItems()						const			{ return _minimumItems;			}
	int				maximumItems()						const			{ return _maximumItems;			}
	QList<QVariant>	defaultValues()						const			{ return _defaultValues;		}
	bool			duplicateWhenAdding()				const			{ return _duplicateWhenAdding;	}

	Json::Value		getTableValueOptions(const ListModel::RowControlsValues& termsWithComponentValues);
signals:
	void			addItem();
	void			removeItem(int index);
	void			nameChanged(int index, QString name);
	void			newItemNameChanged();
	void			addItemManuallyChanged();
	void			minimumItemsChanged();
	void			maximumItemsChanged();
	void			defaultValuesChanged();
	void			duplicateWhenAddingChanged();

public slots:
	GENERIC_SET_FUNCTION(NewItemName,			_newItemName,			newItemNameChanged,				QString			)
	GENERIC_SET_FUNCTION(AddItemManually,		_addItemManually,		addItemManuallyChanged,			bool			)
	GENERIC_SET_FUNCTION(MinimumItems,			_minimumItems,			minimumItemsChanged,			int				)
	GENERIC_SET_FUNCTION(MaximumItems,			_maximumItems,			maximumItemsChanged,			int				)
	GENERIC_SET_FUNCTION(DefaultValues,			_defaultValues,			defaultValuesChanged,			QList<QVariant>	)
	GENERIC_SET_FUNCTION(DuplicateWhenAdding,	_duplicateWhenAdding,	duplicateWhenAddingChanged,		bool			)

protected slots:
	void			termsChangedHandler()								override;
	void			addItemHandler();
	void			removeItemHandler(int index);
	void			nameChangedHandler(int index, QString name);
	void			resetDefaultValue();

protected:
	QString				_makeUnique(const QString& val, int index = -1)	const;
	QString				_makeUnique(const QString& val, const QList<QString>& values, int index = -1) const;
	QString				_changeLastNumber(const QString& val) const;

private:
	ListModelTermsAssigned*		_termsModel				= nullptr;
	QString						_newItemName			= "#";
	bool						_addItemManually		= false,
								_duplicateWhenAdding	= false;
	int							_minimumItems			= 0,
								_maximumItems			= -1;
	QList<QVariant>				_defaultValues;
};

#endif // COMPONENTSLIST_H
