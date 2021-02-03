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

#ifndef TABLEVIEWBASE_H
#define TABLEVIEWBASE_H

#include "jasplistcontrol.h"
#include "boundcontroltableview.h"
#include "listmodeltableviewbase.h"

class TableViewBase : public JASPListControl, public BoundControl
{
	Q_OBJECT

	Q_PROPERTY( ModelType		modelType			READ modelType			WRITE setModelType			NOTIFY modelTypeChanged				)
	Q_PROPERTY( ItemType		itemType			READ itemType			WRITE setItemType			NOTIFY itemTypeChanged				)
	Q_PROPERTY( QString			defaultEmptyValue	READ defaultEmptyValue	WRITE setDefaultEmptyValue	NOTIFY defaultEmptyValueChanged		)
	Q_PROPERTY( int				initialColumnCount	READ initialColumnCount	WRITE setInitialColumnCount	NOTIFY initialColumnCountChanged	)
	Q_PROPERTY( int				initialRowCount		READ initialRowCount	WRITE setInitialRowCount	NOTIFY initialRowCountChanged		)
	Q_PROPERTY( int				columnCount			READ columnCount									NOTIFY columnCountChanged			)
	Q_PROPERTY( int				rowCount			READ rowCount										NOTIFY rowCountChanged				)
	Q_PROPERTY( int				variableCount		READ variableCount									NOTIFY variableCountChanged			)

public:
	TableViewBase(QQuickItem* parent = nullptr);

	void						bindTo(const Json::Value &value)			override	{ _boundControl->bindTo(value);						}
	bool						isJsonValid(const Json::Value& optionValue) override	{ return _boundControl->isJsonValid(optionValue);	}
	void						resetBoundValue()							override	{ return _boundControl->resetBoundValue();			}
	const Json::Value&			boundValue()								override	{ return _boundControl->boundValue();				}
	Json::Value					createJson()								override	{ return _boundControl->createJson();				}
	void						setBoundValue(const Json::Value& value, bool emitChange = true) override	{ return _boundControl->setBoundValue(value, emitChange);	}
	std::vector<std::string>	usedVariables()								override	{ return _boundControl->usedVariables();			}

	ListModel*					model()									const	override { return _tableModel; }
	ListModelTableViewBase*		tableModel()							const			 { return _tableModel; }
	void						setUpModel()									override;
	void						setUp()											override;
	void						rScriptDoneHandler(const QString & result)		override;

	JASPControl::ModelType		modelType()								const				{ return _modelType;					}
	JASPControl::ItemType		itemType()								const				{ return _itemType;						}
	const QString&				defaultEmptyValue()						const				{ return _defaultEmptyValue;			}
	QVariant					defaultValue();
	int							initialColumnCount()					const				{ return _initialColumnCount;			}
	int							initialRowCount()						const				{ return _initialRowCount;				}
	int							rowCount()								const				{ return _tableModel ? _tableModel->rowCount()		: 0; }
	int							columnCount()							const				{ return _tableModel ? _tableModel->columnCount()	: 0; }
	int							variableCount()							const				{ return _tableModel ? _tableModel->variableCount()	: 0; }

signals:
	void						modelTypeChanged();
	void						itemTypeChanged();
	void						defaultEmptyValueChanged();
	void						initialRowCountChanged();
	void						initialColumnCountChanged();
	void						rowCountChanged();
	void						columnCountChanged();
	void						variableCountChanged();

public slots:
	void						refreshMe();

protected slots:
	void						termsChangedHandler()							override;

	GENERIC_SET_FUNCTION(ModelType,				_modelType,				modelTypeChanged,			ModelType	)
	GENERIC_SET_FUNCTION(ItemType,				_itemType,				itemTypeChanged,			ItemType	)
	GENERIC_SET_FUNCTION(DefaultEmptyValue,		_defaultEmptyValue,		defaultEmptyValueChanged,	QString		)
	GENERIC_SET_FUNCTION(InitialRowCount,		_initialRowCount,		initialRowCountChanged,		int			)
	GENERIC_SET_FUNCTION(InitialColumnCount,	_initialColumnCount,	initialColumnCountChanged,	int			)

protected:
	BoundControlTableView		* _boundControl	= nullptr;
	ListModelTableViewBase		* _tableModel	= nullptr;
	
private slots:
	void addColumnSlot();
	void removeColumnSlot(int col);
	void addRowSlot();
	void removeRowSlot(int row);
	void resetSlot();
	void itemChangedSlot(int col, int row, QString value, QString type);

private:
	QString					_defaultEmptyValue;
	ModelType				_modelType				= ModelType::Simple;
	ItemType				_itemType				= ItemType::Double;
	int						_initialRowCount		= 0,
							_initialColumnCount		= 0;
};

#endif // TABLEVIEWBASE_H
