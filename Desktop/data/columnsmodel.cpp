#include "columnsmodel.h"
#include "qquick/jasptheme.h"

QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	//So yes despite this being a "proxy model" it doesn't actually use any data from that model as passed through.
	if(index.row() < 0 || index.row() >= rowCount()) return QVariant();

	switch(role)
	{
	case NameRole:			return _tableModel->columnTitle(index.row());
	case TypeRole:			return "column";
	case IconSourceRole:
		switch(_tableModel->getColumnType(index.row()))
		{
		case columnType::scale:			return JaspTheme::currentIconPath() + "variable-scale.png";
		case columnType::ordinal:		return JaspTheme::currentIconPath() + "variable-ordinal.png";
		case columnType::nominal:		return JaspTheme::currentIconPath() + "variable-nominal.png";
		case columnType::nominalText:	return JaspTheme::currentIconPath() + "variable-nominal-text.png";
		default:						return "";
		}
	case ToolTipRole:
	{
		columnType	colType = _tableModel->getColumnType(index.row());
		QString		usedIn	= colType == columnType::scale ? tr("which can be used in numerical comparisons") : colType == columnType::ordinal ? tr("which can only be used in (in)equivalence, greater and lesser than comparisons") : tr("which can only be used in (in)equivalence comparisons");

		return tr("The '") + _tableModel->columnTitle(index.row()).toString() + tr("'-column ") + usedIn;
	}
	}

	return QVariant();
}

QHash<int, QByteArray> ColumnsModel::roleNames() const
{
	//These should be the same as used in ElementView.qml
	static const auto roles = QHash<int, QByteArray>{
		{ NameRole,					"columnName"},
		{ TypeRole,					"type"},
		{ IconSourceRole,			"columnIcon"},
		{ ToolTipRole,				"toolTip"}
	};

	return roles;
}

void ColumnsModel::onHeaderDataChanged(Qt::Orientation, int, int)
{
	beginResetModel();
	endResetModel();
}

void ColumnsModel::onDataChanged(const QModelIndex &, const QModelIndex &, const QVector<int> &)
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
	emit namesChanged(changeNameColumns);
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
