#include "columnsmodel.h"
#include "qquick/jasptheme.h"

ColumnsModel* ColumnsModel::_singleton = nullptr;

ColumnsModel::ColumnsModel(DataSetTableModel *tableModel) : QAbstractTableModel(tableModel), _tableModel(tableModel)
{
	connect(_tableModel, &DataSetTableModel::headerDataChanged,		this, &ColumnsModel::onHeaderDataChanged);
	connect(_tableModel, &DataSetTableModel::dataChanged,			this, &ColumnsModel::onDataChanged		);
	connect(_tableModel, &DataSetTableModel::modelAboutToBeReset,	this, &ColumnsModel::beginResetModel	);
	connect(_tableModel, &DataSetTableModel::modelReset,				this, &ColumnsModel::endResetModel			);
	connect(_tableModel, &DataSetTableModel::columnTypeChanged,		this, &ColumnsModel::columnTypeChanged	);
	connect(_tableModel, &DataSetTableModel::labelChanged,			this, &ColumnsModel::labelChanged		);
	connect(_tableModel, &DataSetTableModel::labelsReordered,		this, &ColumnsModel::labelsReordered	);

	if (_singleton == nullptr) _singleton = this;
}

QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	//So yes despite this being a "proxy model" it doesn't actually use any data from that model as passed through.
	if(index.row() < 0 || index.row() >= rowCount()) return QVariant();

	switch(role)
	{
	case NameRole:					return _tableModel->columnTitle(index.row());
	case TypeRole:					return int(_tableModel->getColumnType(size_t(index.row())));
	case TypeNameRole:				return tq(columnTypeToString(_tableModel->getColumnType(size_t(index.row()))));
	case IconSourceRole:			return getIconFile(_tableModel->getColumnType(size_t(index.row())), ColumnsModel::DefaultIconType);
	case DisabledIconSourceRole:	return getIconFile(_tableModel->getColumnType(size_t(index.row())), ColumnsModel::DisabledIconType);
	case InactiveIconSourceRole:	return getIconFile(_tableModel->getColumnType(size_t(index.row())), ColumnsModel::InactiveIconType);
	case ToolTipRole:
	{
		columnType	colType = _tableModel->getColumnType(size_t(index.row()));
		QString		usedIn	= colType == columnType::scale ? tr("which can be used in numerical comparisons") : colType == columnType::ordinal ? tr("which can only be used in (in)equivalence, greater and lesser than comparisons") : tr("which can only be used in (in)equivalence comparisons");

		return tr("The '") + _tableModel->columnTitle(index.row()).toString() + tr("'-column ") + usedIn;
	}
	case LabelsRole:	return _tableModel->getColumnLabelsAsStringList(index.row());
	}

	return QVariant();
}

QHash<int, QByteArray> ColumnsModel::roleNames() const
{
	//These should be the same as used in ElementView.qml
	static const auto roles = QHash<int, QByteArray>{
		{ NameRole,					"columnName"			},
		{ TypeRole,					"type"					},
		{ TypeNameRole,				"typeName"				},
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

void ColumnsModel::onHeaderDataChanged(Qt::Orientation, int, int)
{
	refresh();
}

void ColumnsModel::onDataChanged(const QModelIndex &, const QModelIndex &, const QVector<int> &)
{
	refresh();
}

void ColumnsModel::datasetChanged(	QStringList				changedColumns,
									QStringList				missingColumns,
									QMap<QString, QString>	changeNameColumns,
									bool					rowCountChanged,
									bool					hasNewColumns)
{
	if (changeNameColumns.size() > 0)
		emit namesChanged(changeNameColumns);
	else if (missingColumns.size() > 0 || hasNewColumns)
		refresh();
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

int ColumnsModel::rowCount(const QModelIndex &) const
{
	return _tableModel->columnCount();
}

int ColumnsModel::columnCount(const QModelIndex &) const
{
	return 1; //We just show columns
}

//What does headerData even mean here? Doesn't really matter at the moment (20-11-2019)
QVariant ColumnsModel::headerData(int section, Qt::Orientation orientation, int role ) const
{
	return _tableModel->headerData(section, orientation == Qt::Orientation::Vertical ? Qt::Horizontal : Qt::Vertical, role);
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
		case columnType::nominal:		return path+ "variable-nominal.png";
		case columnType::nominalText:	return path + "variable-nominal-text.png";
		default:						return "";
		}
	case ColumnsModel::DisabledIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale-disabled.png";
		case columnType::ordinal:		return path + "variable-ordinal-disabled.png";
		case columnType::nominal:		return path + "variable-nominal-disabled.png";
		case columnType::nominalText:	return path + "variable-nominal-text-disabled.png";
		default:						return "";
		}
	case ColumnsModel::InactiveIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale-inactive.png";
		case columnType::ordinal:		return path + "variable-ordinal-inactive.png";
		case columnType::nominal:		return path + "variable-nominal-inactive.png";
		case columnType::nominalText:	return path + "variable-nominal-text-inactive.png";
		default:						return "";
		}
	}
}
