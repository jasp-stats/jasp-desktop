#include "columnsmodel.h"
#include "qquick/jasptheme.h"

ColumnsModel* ColumnsModel::_singleton = nullptr;

ColumnsModel::ColumnsModel(DataSetTableModel *tableModel) : QTransposeProxyModel(tableModel), _tableModel(tableModel)
{
	setSourceModel(tableModel);

	connect(_tableModel, &DataSetTableModel::columnTypeChanged,		this, &ColumnsModel::columnTypeChanged	);
	connect(_tableModel, &DataSetTableModel::labelChanged,			this, [&](QString col, QString orgLabel, QString newLabel) { emit labelsChanged(col, {std::make_pair(orgLabel, newLabel) }); } );
	connect(_tableModel, &DataSetTableModel::labelsReordered,		this, &ColumnsModel::labelsReordered	);

	if (_singleton == nullptr) _singleton = this;
}

QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	if(index.row() < 0 || index.row() >= rowCount()) return QVariant();

	switch(role)
	{
	case NameRole:					return _tableModel->columnTitle(index.row());
	case TypeRole:					return "column";
	case ColumnTypeRole:			return int(_tableModel->getColumnType(size_t(index.row())));
	case IconSourceRole:			return getIconFile(_tableModel->getColumnType(size_t(index.row())), ColumnsModel::DefaultIconType);
	case DisabledIconSourceRole:	return getIconFile(_tableModel->getColumnType(size_t(index.row())), ColumnsModel::DisabledIconType);
	case InactiveIconSourceRole:	return getIconFile(_tableModel->getColumnType(size_t(index.row())), ColumnsModel::InactiveIconType);
	case ToolTipRole:
	{
		columnType	colType = _tableModel->getColumnType(size_t(index.row()));
		QString		usedIn	= colType == columnType::scale		? tr("which can be used in numerical comparisons and mathematical operations.")
							: colType == columnType::ordinal	? tr("which can only be used in (in)equivalence, greater and lesser than comparisons. Not in mathematical operations as subtraction etc, to do so: try converting to scalar first.")
																: tr("which can only be used in (in)equivalence comparisons. Not in greater/lesser-than comparisons or mathematical operations, to do so: try converting to ordinal or scalar first.");

		return tr("The '") + _tableModel->columnTitle(index.row()).toString() + tr("'-column ") + usedIn;
	}
	case LabelsRole:				return _tableModel->getColumnLabelsAsStringList(index.row());
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
		{ DisabledIconSourceRole,	"columnDisabledIcon"	},
		{ InactiveIconSourceRole,	"columnInactiveIcon"	},
		{ ToolTipRole,				"toolTip"				},
		{ LabelsRole,				"labels"				}
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

QString ColumnsModel::getIconFile(columnType colType, ColumnsModel::IconType type) const
{
	QString path = JaspTheme::currentIconPath();
	switch(type)
	{
	case ColumnsModel::DefaultIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale.png";
		case columnType::ordinal:		return path + "variable-ordinal.png";
		case columnType::nominal:		return path + "variable-nominal.png";
		case columnType::nominalText:	return path + "variable-nominal-text.png";
		default:						return "";
		}
	case ColumnsModel::DisabledIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale-disabled.png";
		case columnType::ordinal:		return path + "variable-ordinal-disabled.png";
		case columnType::nominal:		return path + "variable-nominal-disabled.png";
		case columnType::nominalText:	return path + "variable-nominal-text-inactive.svg";
		default:						return "";
		}
	case ColumnsModel::InactiveIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale-inactive.png";
		case columnType::ordinal:		return path + "variable-ordinal-inactive.png";
		case columnType::nominal:		return path + "variable-nominal-inactive.png";
		case columnType::nominalText:	return path + "variable-nominal-text-inactive.svg";
		default:						return "";
		}
	}

	return ""; //We are never getting here but GCC isn't convinced
}
