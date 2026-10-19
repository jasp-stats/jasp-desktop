#include "varinfomodelproxy.h"
#include "dataenums.h"
#include "dataset.h"
#include "qutils.h"
#include "log.h"

VarInfoModelProxy::VarInfoModelProxy(FilteredData * filteredData) 
: QAbstractTableModel(filteredData), 
  _filteredData(filteredData)
{
	connect(_filteredData,	&FilteredData::modelReset,				this, &VarInfoModelProxy::refresh		);
	connect(_filteredData,	&FilteredData::dataChanged,				this, &VarInfoModelProxy::refresh		);
}


QVariant VarInfoModelProxy::data(const QModelIndex &index, int role) const
{
	QString				colName		=									 _filteredData->headerData(index.row(), Qt::Horizontal, int(dataPkgRoles::name					)).toString();
	columnType			colType		= static_cast<columnType>			(_filteredData->headerData(index.row(), Qt::Horizontal, int(dataPkgRoles::columnType			)).toInt());
	computedColumnType	codeType	= static_cast<computedColumnType>	(_filteredData->headerData(index.row(), Qt::Horizontal, int(dataPkgRoles::computedColumnType	)).toInt());

	switch(role)
	{
	case NameRole:					return colName;
	case TypeRole:					return "column";
	case ColumnTypeRole:			return int(colType);
	case ComputedColumnTypeRole:	return int(codeType);
	case IconSourceRole:			return getIconFilename(colType, varIconType::DefaultIconType);
	case ToolTipRole:				return QColumnUtils::getTypeFriendly(colType);
	}
	
	return _filteredData->data(_filteredData->index(index.column(), index.row()), role);
}

int VarInfoModelProxy::rowCount(const QModelIndex &) const
{
	return _filteredData->columnCount();
}

int VarInfoModelProxy::columnCount(const QModelIndex &) const
{
	return 1;
}

QHash<int, QByteArray> VarInfoModelProxy::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{ NameRole,					"columnName"			},
		{ TypeRole,					"type"					},
		{ ColumnTypeRole,			"columnType"			},
		{ ComputedColumnTypeRole,	"computedColumnType"	},
		{ IconSourceRole,			"columnIcon"			},
		{ ToolTipRole,				"toolTip"				}
	};

	return roles;
}
