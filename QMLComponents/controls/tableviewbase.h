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
#include "boundcontrols/boundcontroltableview.h"
#include "models/listmodeltableviewbase.h"

class TableViewBase : public JASPListControl, public BoundControl
{
	Q_OBJECT

	Q_PROPERTY( ModelType		modelType			READ modelType				WRITE setModelType				NOTIFY modelTypeChanged					)
	Q_PROPERTY( ItemType		itemType			READ itemType				WRITE setItemType				NOTIFY itemTypeChanged					)
	Q_PROPERTY( QVariantList	itemTypePerColumn	READ itemTypePerColumn		WRITE setItemTypePerColumn		NOTIFY itemTypePerColumnChanged			)
	Q_PROPERTY( QVariantList	itemTypePerRow		READ itemTypePerRow			WRITE setItemTypePerRow			NOTIFY itemTypePerRowChanged			)
	Q_PROPERTY( QVariant		defaultValue		READ defaultValue			WRITE setDefaultValue			NOTIFY defaultValueChanged				)
	Q_PROPERTY( QVariant		initialValuesSource	READ initialValuesSource	WRITE setInitialValuesSource	NOTIFY initialValuesSourceChanged		)
	Q_PROPERTY( int				initialColumnCount	READ initialColumnCount		WRITE setInitialColumnCount		NOTIFY initialColumnCountChanged		)
	Q_PROPERTY( int				initialRowCount		READ initialRowCount		WRITE setInitialRowCount		NOTIFY initialRowCountChanged			)
	Q_PROPERTY( int				columnCount			READ columnCount											NOTIFY columnCountChanged				)
	Q_PROPERTY( int				rowCount			READ rowCount												NOTIFY rowCountChanged					)
	Q_PROPERTY( int				variableCount		READ variableCount											NOTIFY variableCountChanged				)
	Q_PROPERTY( int				minRow				READ minRow					WRITE setMinRow					NOTIFY minRowChanged					)
	Q_PROPERTY( int				maxRow				READ maxRow					WRITE setMaxRow					NOTIFY maxRowChanged					)
	Q_PROPERTY( int				minColumn			READ minColumn				WRITE setMinColumn				NOTIFY minColumnChanged					)
	Q_PROPERTY( int				maxColumn			READ maxColumn				WRITE setMaxColumn				NOTIFY maxColumnChanged					)
	Q_PROPERTY( QStringList		columnNames			READ columnNames			WRITE setColumnNames			NOTIFY columnNamesChanged				)
	Q_PROPERTY( QStringList		rowNames			READ rowNames				WRITE setRowNames				NOTIFY rowNamesChanged					)
	Q_PROPERTY( bool			updateSource		READ updateSource			WRITE setUpdateSource			NOTIFY updateSourceChanged				)

public:
	TableViewBase(QQuickItem* parent = nullptr);

	void						bindTo(const Json::Value &value)					override	{ _boundControl->bindTo(value);							}
	bool						isJsonValid(const Json::Value& optionValue) const	override	{ return _boundControl->isJsonValid(optionValue);		}
	void						resetBoundValue()									override	{ return _boundControl->resetBoundValue();				}
	const Json::Value&			boundValue()								const	override	{ return _boundControl->boundValue();					}
	Json::Value					createJson()								const	override	{ return _boundControl->createJson();					}
	Json::Value					createMeta()								const	override	{ return _boundControl->createMeta();					}
	const Json::Value&			defaultBoundValue()							const	override	{ return _boundControl->defaultBoundValue();			}
	void						setDefaultBoundValue(const Json::Value& defaultValue) override	{ _boundControl->setDefaultBoundValue(defaultValue);	}

	void						setBoundValue(const Json::Value& value, 
											  bool emitChange = true)				override	{ return _boundControl->setBoundValue(value, emitChange); }

	ListModel*					model()										const	override	{ return _tableModel;									}
	ListModelTableViewBase*		tableModel()								const				{ return _tableModel;									}
	void						setUpModel()										override;
	void						setUp()												override;
	void						rScriptDoneHandler(const QString & result)			override;

	ItemType itemTypePerItem(int col = -1, int row = -1) const;

	JASPControl::ModelType		modelType()									const				{ return _modelType;									}
	JASPControl::ItemType		itemType()									const				{ return _itemType;										}
	QVariant					defaultValue(int colIndex = -1, int rowIndex = -1);
	QVariantList				itemTypePerRow()							const				{ QVariantList l; for (auto t : _itemTypePerRow) l.append(int(t)); return l;	}
	QVariantList				itemTypePerColumn()							const				{ QVariantList l; for (auto t : _itemTypePerColumn) l.append(int(t)); return l;	}
	QVariant					initialValuesSource()						const				{ return _initialValuesSource;							}
	JASPListControl*			initialValuesControl()						const				{ return _initialValuesControl;							}
	int							initialColumnCount()						const				{ return _initialColumnCount;							}
	int							initialRowCount()							const				{ return _initialRowCount;								}
	int							rowCount()									const				{ return _tableModel ? _tableModel->rowCount()		: 0; }
	int							columnCount()								const				{ return _tableModel ? _tableModel->columnCount()	: 0; }
	int							variableCount()								const				{ return _tableModel ? _tableModel->variableCount()	: 0; }
	int							minRow()									const				{ return _minRow;										}
	int							maxRow()									const				{ return _maxRow;										}
	int							minColumn()									const				{ return _minColumn;									}
	int							maxColumn()									const				{ return _maxColumn;									}
	QStringList					columnNames()								const				{ return _columnNames;									}
	QStringList					rowNames()									const				{ return _rowNames;										}
	bool						updateSource()								const				{ return _updateSource;									}
	std::vector<std::string>	usedVariables()								const	override;

	Q_INVOKABLE void addColumn(int col = -1, bool left = true);
	Q_INVOKABLE void removeColumn(int col);
	Q_INVOKABLE void addRow();
	Q_INVOKABLE void removeRow(int row);
	Q_INVOKABLE void reset();
	Q_INVOKABLE void itemChanged(int col, int row, QString value, QString type);


signals:
	void						modelTypeChanged();
	void						itemTypeChanged();
	void						itemTypePerRowChanged();
	void						itemTypePerColumnChanged();
	void						defaultValueChanged();
	void						initialRowCountChanged();
	void						initialColumnCountChanged();
	void						initialValuesSourceChanged();
	void						rowCountChanged();
	void						columnCountChanged();
	void						variableCountChanged();
	void						minRowChanged();
	void						maxRowChanged();
	void						minColumnChanged();
	void						maxColumnChanged();
	void						columnNamesChanged();
	void						rowNamesChanged();
	void						updateSourceChanged();

public slots:
	void						refreshMe();

protected slots:
	void						termsChangedHandler()							override;

	GENERIC_SET_FUNCTION(ModelType,				_modelType,				modelTypeChanged,			ModelType	)
	GENERIC_SET_FUNCTION(ItemType,				_itemType,				itemTypeChanged,			ItemType	)
	GENERIC_SET_FUNCTION(DefaultValue,			_defaultValue,			defaultValueChanged,		QVariant	)
	GENERIC_SET_FUNCTION(InitialRowCount,		_initialRowCount,		initialRowCountChanged,		int			)
	GENERIC_SET_FUNCTION(InitialColumnCount,	_initialColumnCount,	initialColumnCountChanged,	int			)
	GENERIC_SET_FUNCTION(InitialValuesSource,	_initialValuesSource,	initialValuesSourceChanged,	QVariant	)
	GENERIC_SET_FUNCTION(MinRow,				_minRow,				minRowChanged,				int			)
	GENERIC_SET_FUNCTION(MaxRow,				_maxRow,				maxRowChanged,				int			)
	GENERIC_SET_FUNCTION(MinColumn,				_minColumn,				minColumnChanged,			int			)
	GENERIC_SET_FUNCTION(MaxColumn,				_maxColumn,				maxColumnChanged,			int			)
	GENERIC_SET_FUNCTION(ColumnNames,			_columnNames,			columnNamesChanged,			QStringList	)
	GENERIC_SET_FUNCTION(RowNames,				_rowNames,				rowNamesChanged,			QStringList	)
	GENERIC_SET_FUNCTION(UpdateSource,			_updateSource,			updateSourceChanged,		bool		)

	void	setItemTypePerRow(QVariantList list);
	void	setItemTypePerColumn(QVariantList list);

protected:
	BoundControlTableView		* _boundControl	= nullptr;
	ListModelTableViewBase		* _tableModel	= nullptr;
	
private slots:
	void setInitialValuesControl();

private:
	QVariant				_defaultValue;
	ModelType				_modelType				= ModelType::Simple;
	ItemType				_itemType				= ItemType::Double;
	QList<ItemType>			_itemTypePerRow,
							_itemTypePerColumn;
	int						_initialRowCount		= 0,
							_initialColumnCount		= 0,
							_minRow					= 0,
							_minColumn				= 0,
							_maxRow					= -1,
							_maxColumn				= -1;
	QStringList				_columnNames,
							_rowNames;
	QVariant				_initialValuesSource;
	JASPListControl*		_initialValuesControl	= nullptr;
	bool					_updateSource			= false;
};

#endif // TABLEVIEWBASE_H
