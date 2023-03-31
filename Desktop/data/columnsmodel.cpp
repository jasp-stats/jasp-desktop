#include "columnsmodel.h"
#include "jasptheme.h"
#include "log.h"
#include "utilities/qutils.h"

ColumnsModel* ColumnsModel::_singleton = nullptr;

ColumnsModel::ColumnsModel(DataSetTableModel *tableModel) : QTransposeProxyModel(tableModel), _tableModel(tableModel)
{
	setSourceModel(tableModel);

	connect(_tableModel, &DataSetTableModel::columnTypeChanged,		this, &ColumnsModel::columnTypeChanged	);
	connect(_tableModel, &DataSetTableModel::labelChanged,			this, [&](QString col, QString orgLabel, QString newLabel) { emit labelsChanged(col, {std::make_pair(orgLabel, newLabel) }); } );
	connect(_tableModel, &DataSetTableModel::labelsReordered,		this, &ColumnsModel::labelsReordered	);

	if (_singleton == nullptr)
	{
		_singleton = this;
		VariableInfo* info = new VariableInfo(this);
		connect(this, &ColumnsModel::namesChanged,		info, &VariableInfo::namesChanged		);
		connect(this, &ColumnsModel::columnsChanged,	info, &VariableInfo::columnsChanged		);
		connect(this, &ColumnsModel::columnTypeChanged, info, &VariableInfo::columnTypeChanged	);
		connect(this, &ColumnsModel::labelsChanged,		info, &VariableInfo::labelsChanged		);
		connect(this, &ColumnsModel::labelsReordered,	info, &VariableInfo::labelsReordered	);
	}
}

QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	if(index.row() < 0 || index.row() >= rowCount()) return QVariant();

	switch(role)
	{
	case NameRole:					return _tableModel->columnTitle(index.row());
	case TypeRole:					return "column";
	case ColumnTypeRole:			return int(_tableModel->getColumnType(size_t(index.row())));
	case IconSourceRole:			return VariableInfo::getIconFile(_tableModel->getColumnType(size_t(index.row())), VariableInfo::DefaultIconType);
	case ToolTipRole:
	{
		columnType	colType = _tableModel->getColumnType(size_t(index.row()));
		QString		usedIn	= colType == columnType::scale		? tr("which can be used in numerical comparisons and mathematical operations.")
							: colType == columnType::ordinal	? tr("which can only be used in (in)equivalence, greater and lesser than comparisons. Not in mathematical operations as subtraction etc, to do so: try converting to scalar first.")
																: tr("which can only be used in (in)equivalence comparisons. Not in greater/lesser-than comparisons or mathematical operations, to do so: try converting to ordinal or scalar first.");

		return tr("The '") + _tableModel->columnTitle(index.row()).toString() + tr("'-column ") + usedIn;
	}
	}

	return QVariant();
}

QHash<int, QByteArray> ColumnsModel::roleNames() const
{
	//These should be the same as used in ElementView.qml
	static const auto roles = QHash<int, QByteArray>{
		{ NameRole,					"columnName"			},
		{ TypeRole,					"type"					},
		{ ColumnTypeRole,			"columnType"			},
		{ IconSourceRole,			"columnIcon"			},
		{ ToolTipRole,				"toolTip"				}
	};

	return roles;
}

void ColumnsModel::refresh()
{
	beginResetModel();
	endResetModel();
}

void ColumnsModel::datasetChanged(	QStringList				changedColumns,
									QStringList				missingColumns,
									QMap<QString, QString>	changeNameColumns,
									bool					rowCountChanged,
									bool					hasNewColumns)
{
	if (missingColumns.size() > 0 || hasNewColumns)
		refresh();
	else
	{
		if (changeNameColumns.size() > 0)
			emit namesChanged(changeNameColumns);
		else if (changedColumns.size() > 0 || rowCountChanged)
		{
			if (rowCountChanged)
			{
				changedColumns.clear();
				for (int i = 0; i < rowCount(); i++)
					changedColumns.push_back(_tableModel->columnTitle(i).toString());
			}
			emit columnsChanged(changedColumns);
		}
	}
}

QStringList ColumnsModel::getColumnNames() const
{
	QStringList result;

	int rows = rowCount();
	for (int i = 0; i < rows; i++)
		result.append(data(index(i, 0), NameRole).toString());

	return result;
}

QVariant ColumnsModel::provideInfo(VariableInfo::InfoType info, const QString& colName, int row) const
{
	ColumnsModel* colModel = ColumnsModel::singleton();
	if (!colModel) return QVariant();

	try
	{
		int colIndex = colName.isEmpty() ? 0 : colModel->getColumnIndex(fq(colName));

		if (colIndex < 0)
			return "";

		switch(info)
		{
		case VariableInfo::VariableType:				return	int(_tableModel->getColumnType(colIndex));
		case VariableInfo::VariableTypeName:			return	columnTypeToQString(_tableModel->getColumnType(colIndex));
		case VariableInfo::VariableTypeIcon:			return	VariableInfo::getIconFile(_tableModel->getColumnType(colIndex), VariableInfo::DefaultIconType);
		case VariableInfo::VariableTypeDisabledIcon:	return	VariableInfo::getIconFile(_tableModel->getColumnType(colIndex), VariableInfo::DisabledIconType);
		case VariableInfo::VariableTypeInactiveIcon:	return	VariableInfo::getIconFile(_tableModel->getColumnType(colIndex), VariableInfo::InactiveIconType);
		case VariableInfo::Labels:						return	_tableModel->getColumnLabelsAsStringList(colIndex);
		case VariableInfo::StringValues:				return	_tableModel->getColumnValuesAsStringList(colIndex);
		case VariableInfo::DoubleValues:				return	_tableModel->getColumnValuesAsDoubleList(colIndex);
		case VariableInfo::NameRole:					return	ColumnsModel::NameRole;
		case VariableInfo::RowCount:					return	_tableModel->rowCount();
		case VariableInfo::Value:						return	_tableModel->data(_tableModel->index(row, colIndex));
		case VariableInfo::MaxWidth:					return	int(_tableModel->getMaximumColumnWidthInCharacters(colIndex));
		case VariableInfo::SignalsBlocked:				return	_tableModel->synchingData();
		case VariableInfo::VariableNames:				return	getColumnNames();
		}
	}
	catch(columnNotFound & e) {} //just return an empty QVariant right?
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}
	return QVariant("");
}
