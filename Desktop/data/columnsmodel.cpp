#include "columnsmodel.h"
#include "jasptheme.h"
#include "log.h"
#include "utilities/qutils.h"

ColumnsModel * ColumnsModel::_singleton = nullptr;

ColumnsModel::ColumnsModel(DataSetTableModel *tableModel) 
: QTransposeProxyModel(tableModel), _tableModel(tableModel)
{
	assert(!_singleton);
	_singleton = this;
	
	setSourceModel(tableModel);

	connect(_tableModel, &DataSetTableModel::columnTypeChanged,		this, &ColumnsModel::columnTypeChanged	);
	connect(_tableModel, &DataSetTableModel::labelChanged,			this, [&](QString col, QString orgLabel, QString newLabel) { emit labelsChanged(col, {std::make_pair(orgLabel, newLabel) }); } );
	connect(_tableModel, &DataSetTableModel::labelsReordered,		this, &ColumnsModel::labelsReordered	);

	auto * info = new VariableInfo(_singleton);
	
	connect(this, &ColumnsModel::namesChanged,		info, &VariableInfo::namesChanged		);
	connect(this, &ColumnsModel::columnsChanged,	info, &VariableInfo::columnsChanged	);
	connect(this, &ColumnsModel::columnTypeChanged, info, &VariableInfo::columnTypeChanged	);
	connect(this, &ColumnsModel::labelsChanged,		info, &VariableInfo::labelsChanged		);
	connect(this, &ColumnsModel::labelsReordered,	info, &VariableInfo::labelsReordered	);

}

ColumnsModel::~ColumnsModel()
{ 
	if(_singleton == this) 
		_singleton = nullptr;
}

QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	if(index.row() < 0 || index.row() >= rowCount()) return QVariant();

	QString		colName = QTransposeProxyModel::data(index, int(DataSetPackage::specialRoles::name)).toString();
	columnType	colType = static_cast<columnType>(QTransposeProxyModel::data(index, int(DataSetPackage::specialRoles::columnType)).toInt());

	switch(role)
	{
	case NameRole:					return colName;
	case TypeRole:					return "column";
	case ColumnTypeRole:			return int(colType);
	case IconSourceRole:			return VariableInfo::getIconFile(colType, VariableInfo::DefaultIconType);
	case ToolTipRole:
	{
		QString		usedIn	= colType == columnType::scale		? tr("which can be used in numerical comparisons and mathematical operations.")
							: colType == columnType::ordinal	? tr("which can only be used in (in)equivalence, greater and lesser than comparisons. Not in mathematical operations as subtraction etc, to do so: try converting to scalar first.")
																: tr("which can only be used in (in)equivalence comparisons. Not in greater/lesser-than comparisons or mathematical operations, to do so: try converting to ordinal or scalar first.");

		return tr("The '") + colName + tr("'-column ") + usedIn;
	}
	}

	return QVariant();
}

QVariant ColumnsModel::provideInfo(VariableInfo::InfoType info, const QString& colName, int row) const
{
	ColumnsModel* colModel = ColumnsModel::singleton();

	if (!colModel)
		return QVariant();

	try
	{
		int colIndex = colName.isEmpty() ? 0 : colModel->getColumnIndex(fq(colName));

		if (colIndex < 0)
			return QVariant();

		//remember, the model is transposed:
		QModelIndex qIndex = index(colIndex, 0);

		int			colTypeInt	= data(qIndex, ColumnsModel::ColumnTypeRole).toInt();
		columnType	colTypeHere	= static_cast<columnType>(colTypeInt);

		switch(info)
		{
		case VariableInfo::VariableType:				return	colTypeInt;
		case VariableInfo::VariableTypeName:			return	columnTypeToQString(colTypeHere);
		case VariableInfo::VariableTypeIcon:			return	VariableInfo::getIconFile(colTypeHere, VariableInfo::DefaultIconType);
		case VariableInfo::VariableTypeDisabledIcon:	return	VariableInfo::getIconFile(colTypeHere, VariableInfo::DisabledIconType);
		case VariableInfo::VariableTypeInactiveIcon:	return	VariableInfo::getIconFile(colTypeHere, VariableInfo::InactiveIconType);
		case VariableInfo::Labels:						return	QTransposeProxyModel::data(qIndex, int(DataSetPackage::specialRoles::labelsStrList));
		case VariableInfo::StringValues:				return	QTransposeProxyModel::data(qIndex, int(DataSetPackage::specialRoles::valuesStrList));
		case VariableInfo::DoubleValues:				return	QTransposeProxyModel::data(qIndex, int(DataSetPackage::specialRoles::valuesDblList));
		case VariableInfo::NameRole:					return	data(qIndex, ColumnsModel::NameRole);
		case VariableInfo::RowCount:					return	rowCount();
		case VariableInfo::Value:						return	QTransposeProxyModel::data(qIndex, int(DataSetPackage::specialRoles::value));
		case VariableInfo::MaxWidth:					return	QTransposeProxyModel::headerData(colIndex, Qt::Horizontal, int(DataSetPackage::specialRoles::maxColString)).toInt();
		case VariableInfo::SignalsBlocked:				return	_tableModel->synchingData();
		case VariableInfo::VariableNames:				return	getColumnNames();
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
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

int ColumnsModel::rowCount(const QModelIndex & p) const
{
	return QTransposeProxyModel::rowCount(p);
}


QStringList ColumnsModel::getColumnNames() const
{
	QStringList result;

	int rows = rowCount();
	for (int i = 0; i < rows; i++)
		result.append(data(index(i, 0), NameRole).toString());

	return result;
}

void ColumnsModel::datasetChanged(  QStringList                             changedColumns,
									QStringList                             missingColumns,
									QMap<QString, QString>					changeNameColumns,
									bool                                    rowCountChanged,
									bool                                    hasNewColumns)
{
	   if(! (missingColumns.size() > 0 || hasNewColumns))
	   {
			   if (changeNameColumns.size() > 0)
					   emit namesChanged(changeNameColumns);
			   else if (changedColumns.size() > 0 || rowCountChanged)
			   {
					   if (rowCountChanged)
					   {
							   changedColumns.clear();
							   for (int i = 0; i < rowCount(); i++)
									   changedColumns.push_back(data(index(i, 0), int(dataPkgRoles::name)).toString());
					   }
					   emit columnsChanged(changedColumns);
			   }
	   }
}
