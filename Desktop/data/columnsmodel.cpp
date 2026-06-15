#include "columnsmodel.h"
#include "log.h"
#include "utilities/qutils.h"
#include "mainwindow.h"

ColumnsModel * ColumnsModel::_singleton = nullptr;

ColumnsModel::ColumnsModel(DataSetTableModel *tableModel) 
: QAbstractTableModel(tableModel), _tableModel(tableModel)
{
	assert(!_singleton);
	_singleton = this;
	
	connect(_tableModel, &DataSetTableModel::columnTypeChanged,		this, [&](QString col, int) { emit columnTypeChanged(col); });
	connect(_tableModel, &DataSetTableModel::labelChanged,			this, [&](QString col, QString orgLabel, QString newLabel) { emit labelsChanged(col, {std::make_pair(orgLabel, newLabel) }); } );
	connect(_tableModel, &DataSetTableModel::labelsReordered,		this, &ColumnsModel::labelsReordered	);
	connect(_tableModel, &DataSetTableModel::emptyValuesChanged,	this, &ColumnsModel::dataSetChanged		);
	connect(_tableModel, &DataSetTableModel::modelReset,			this, &ColumnsModel::refresh			);
	connect(_tableModel, &DataSetTableModel::dataChanged,			this, &ColumnsModel::refresh			);
	

	auto * info = new VariableInfo(_singleton);

	connect(this, &ColumnsModel::columnNamesChanged,					info, &VariableInfo::variableNamesChanged	);
	connect(this, &ColumnsModel::columnsChanged,						info, &VariableInfo::variablesChanged		);
	connect(this, &ColumnsModel::columnTypeChanged,						this, [this] (QString colName)
		{
			Term term(colName, columnType(data(index(getColumnIndex(fq(colName)), 0), ColumnsModel::ColumnTypeRole).toInt()));
			emit VariableInfo::info()->variableTypeChanged(term);
		} );

	connect(this,						&ColumnsModel::labelsChanged,				info, &VariableInfo::labelsChanged			);
	connect(this,						&ColumnsModel::labelsReordered,				info, &VariableInfo::labelsReordered		);
	connect(this,						&ColumnsModel::filterChanged,				info, &VariableInfo::filterChanged			);
	connect(this,						&ColumnsModel::dataSetChanged,				info, &VariableInfo::dataSetChanged			);
	connect(this,						&QAbstractTableModel::modelReset,			info, &VariableInfo::rowCountChanged		);
	connect(_tableModel,				&DataSetTableModel::columnsInserted,		info, &VariableInfo::rowCountChanged		);
	connect(_tableModel,				&DataSetTableModel::columnsRemoved,			info, &VariableInfo::rowCountChanged		);
	connect(MainWindow::singleton(),	&MainWindow::dataAvailableChanged,			info, &VariableInfo::dataAvailableChanged	);
}

ColumnsModel::~ColumnsModel()
{ 
	if(_singleton == this) 
		_singleton = nullptr;
}

QString ColumnsModel::getColumnIcon(int colType) const
{
	return getColumnIcon(columnType(colType));
}

QString ColumnsModel::getColumnIcon(int colType, bool isTransformed) const
{
	return !isTransformed ? getColumnIcon(colType) : getColumnIconTransform(colType);
}

QString ColumnsModel::getColumnIcon(columnType colType) const
{
	return VariableInfo::getIconFile(colType, VariableInfo::DefaultIconType);
}

QString ColumnsModel::getColumnDescription(const QString &name) const
{
	return provideInfo(VariableInfo::ColumnDescription, name).toString().trimmed();
}

QString ColumnsModel::getColumnIconTransform(int colType) const
{
	return getColumnIconTransform(columnType(colType));
}

QString ColumnsModel::getColumnIconTransform(columnType colType) const
{
	return VariableInfo::getIconFile(colType, VariableInfo::TransformedIconType);
}

int ColumnsModel::getColumnType(const QString & columnName) const
{
	int index = ColumnsModel::singleton()->getColumnIndex(fq(columnName));
	
	if(index == -1)
		return int(columnType::unknown);
	
	return ColumnsModel::singleton()->data(ColumnsModel::singleton()->index(index, 0), ColumnTypeRole).toInt();
}

QString ColumnsModel::getColumnTransformedToolTip(const QString &name, int transformedTo) const
{
	return 	getColumnTransformedToolTip(name, columnType(transformedTo));
}

QString ColumnsModel::getColumnTransformedToolTip(const QString &name, columnType chosenType) const
{
	columnType	realType	= columnType(getColumnType(name));
	
	if(ColumnsModel::singleton()->getColumnIndex(fq(name)) == -1 || chosenType == realType)
		return "";
	
	VariableInfo::InfoType		previewType;
	
	switch(chosenType)
	{
	default:					previewType = VariableInfo::PreviewScale;		break;
	case columnType::ordinal:	previewType	= VariableInfo::PreviewOrdinal;		break;
	case columnType::nominal:	previewType	= VariableInfo::PreviewNominal;		break;
	}
	
	return provideInfo(previewType, name).toString();
	
}


QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	QString				colName		=									 _tableModel->headerData(index.row(), Qt::Horizontal, int(DataSetPackage::specialRoles::name				)).toString();
	columnType			colType		= static_cast<columnType>			(_tableModel->headerData(index.row(), Qt::Horizontal, int(DataSetPackage::specialRoles::columnType			)).toInt());
	computedColumnType	codeType	= static_cast<computedColumnType>	(_tableModel->headerData(index.row(), Qt::Horizontal, int(DataSetPackage::specialRoles::computedColumnType	)).toInt());

	switch(role)
	{
	case NameRole:					return colName;
	case TypeRole:					return "column";
	case ColumnTypeRole:			return int(colType);
	case ComputedColumnTypeRole:	return int(codeType);
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

int ColumnsModel::rowCount(const QModelIndex &) const
{
	return _tableModel->columnCount();
}

int ColumnsModel::columnCount(const QModelIndex &) const
{
	return 1;
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

		QModelIndex qColIndex	= index(colIndex, 0),
					tableCIndex	= _tableModel->index(0, colIndex),
					tableVIndex	= _tableModel->index(row, colIndex);

		//columnType	colTypeHere	= static_cast<columnType>(colTypeInt);

		switch(info)
		{
		case VariableInfo::VariableType:				return					data(qColIndex, ColumnsModel::ColumnTypeRole).toInt();
		case VariableInfo::NameRole:					return					ColumnsModel::NameRole;
		
		case VariableInfo::DoubleValues:				return	_tableModel->	data(tableCIndex,						int(DataSetPackage::specialRoles::valuesDblList));
		case VariableInfo::TotalNumericValues:			return	_tableModel->	data(tableCIndex,						int(DataSetPackage::specialRoles::nonFilteredNumericValuesCount));
		case VariableInfo::TotalLevels:					return	_tableModel->	data(tableCIndex,						int(DataSetPackage::specialRoles::nonFilteredLevels)).toStringList().length();
		case VariableInfo::Labels:						return	_tableModel->	data(tableCIndex,						int(DataSetPackage::specialRoles::nonFilteredLevels));
		case VariableInfo::DataSetValues:				return	_tableModel->	data(tableCIndex,						int(DataSetPackage::specialRoles::valuesStrList));
		case VariableInfo::DataSetRowCount:				return  _tableModel->	rowCount();
		case VariableInfo::SignalsBlocked:				return	_tableModel->	synchingData();
		case VariableInfo::DataSetValue:				return	_tableModel->	data(tableVIndex,						int(DataSetPackage::specialRoles::value));
		
		case VariableInfo::VariableNames:				return	getColumnNames();
		case VariableInfo::DataAvailable:				return	MainWindow::singleton()->dataAvailable();
		
		case VariableInfo::MaxWidth:					return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(DataSetPackage::specialRoles::maxColString)).toInt();
		case VariableInfo::PreviewScale:				return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(DataSetPackage::specialRoles::previewScale));
		case VariableInfo::PreviewOrdinal:				return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(DataSetPackage::specialRoles::previewOrdinal));
		case VariableInfo::PreviewNominal:				return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(DataSetPackage::specialRoles::previewNominal));
		case VariableInfo::ColumnDescription:			return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(DataSetPackage::specialRoles::description));
		case VariableInfo::DataSetPointer:				return	QVariant::fromValue<void*>(DataSetPackage::pkg()->dataSet());
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return QVariant();
}

bool ColumnsModel::absorbInfo(VariableInfo::InfoType info, const QString &colName, int row, QVariant value)
{
	ColumnsModel* colModel = ColumnsModel::singleton();

	if (!colModel)
		return false;

	try
	{
		int colIndex = colModel->getColumnIndex(fq(colName));

		if (colIndex < 0)
			return false;

		QModelIndex qColIndex	= _tableModel->index(0, colIndex),
					qValIndex	= _tableModel->index(row, colIndex);

		switch(info)
		{
		default:										return	false;
		case VariableInfo::DataSetValue:				return	_tableModel->setData(qValIndex, value,	int(DataSetPackage::specialRoles::value));
		case VariableInfo::DataSetValues:				return	_tableModel->setData(qColIndex, value,	int(DataSetPackage::specialRoles::valuesStrList));
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return false;
}

QHash<int, QByteArray> ColumnsModel::roleNames() const
{
	//These should be the same as used in ElementView.qml
	static const auto roles = QHash<int, QByteArray>{
		{ NameRole,					"columnName"			},
		{ TypeRole,					"type"					},
		{ ColumnTypeRole,			"columnType"			},
		{ ComputedColumnTypeRole,	"computedColumnType"	},
		{ IconSourceRole,			"columnIcolumnTypeUsercon"			},
		{ ToolTipRole,				"toolTip"				}
	};

	return roles;
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
					   emit columnNamesChanged(changeNameColumns);
			   else if (changedColumns.size() > 0 || rowCountChanged)
			   {
					   if (rowCountChanged)
					   {
							   changedColumns.clear();
							   for (int i = 0; i < rowCount(); i++)
									   changedColumns.push_back(data(index(i, 0), NameRole).toString());
					   }
					   emit columnsChanged(changedColumns);
			   }
	   }
	   
	emit dataSetChanged(); //For VariableInfoProvider and listeners
}

