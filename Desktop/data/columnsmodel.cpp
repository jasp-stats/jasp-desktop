#include "log.h"
#include "qutils.h"
#include "jasptheme.h"
#include "dataenums.h"
#include "mainwindow.h"
#include "columnsmodel.h"
#include "timers.h"

ColumnsModel * ColumnsModel::_singleton = nullptr;

ColumnsModel::ColumnsModel(DataSetTableModel *tableModel) 
: QAbstractTableModel(tableModel), _tableModel(tableModel)
{
	assert(!_singleton);
	_singleton = this;
	
	connect(_tableModel, &DataSetTableModel::columnTypeChanged,		this, &ColumnsModel::columnTypeChanged	);
	connect(_tableModel, &DataSetTableModel::labelChanged,			this, [&](const Column * col, QString orgLabel, QString newLabel) { emit labelsChanged(col->nameQ(), QMap<QString, QString>{std::make_pair(orgLabel, newLabel) }); } );
	connect(_tableModel, &DataSetTableModel::labelsReordered,		this, &ColumnsModel::labelsReordered	);
	connect(_tableModel, &DataSetTableModel::emptyValuesChanged,	this, &ColumnsModel::dataSetChanged		);
	connect(_tableModel, &DataSetTableModel::modelReset,			this, &ColumnsModel::refresh			);
	connect(_tableModel, &DataSetTableModel::dataChanged,			this, &ColumnsModel::refresh			);
	
	auto * info = new VariableInfo(_singleton);

	connect(this, &ColumnsModel::columnNamesChanged,					info, &VariableInfo::variableNamesChanged	);
	connect(this, &ColumnsModel::columnsChanged,						info, &VariableInfo::variablesChanged		);

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
	return JaspTheme::currentIconPath() + "/"+ getIconFilename(colType, varIconType::DefaultIconType);
}

QString ColumnsModel::getColumnDescription(const QString &name) const
{
	return provideInfo(varInfoType::ColumnDescription, name).toString().trimmed();
}

QString ColumnsModel::getColumnIconTransform(int colType) const
{
	return getColumnIconTransform(columnType(colType));
}

QString ColumnsModel::getColumnIconTransform(columnType colType) const
{
	return JaspTheme::currentIconPath() + "/"+ getIconFilename(colType, varIconType::TransformedIconType);
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
	
	varInfoType		previewType;
	
	switch(chosenType)
	{
	default:					previewType = varInfoType::PreviewScale;		break;
	case columnType::ordinal:	previewType	= varInfoType::PreviewOrdinal;		break;
	case columnType::nominal:	previewType	= varInfoType::PreviewNominal;		break;
	}
	
	return provideInfo(previewType, name).toString();
	
}


QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	QString				colName		=									 _tableModel->headerData(index.row(), Qt::Horizontal, int(dataPkgRoles::name				)).toString();
	columnType			colType		= static_cast<columnType>			(_tableModel->headerData(index.row(), Qt::Horizontal, int(dataPkgRoles::columnType			)).toInt());
	computedColumnType	codeType	= static_cast<computedColumnType>	(_tableModel->headerData(index.row(), Qt::Horizontal, int(dataPkgRoles::computedColumnType	)).toInt());

	switch(role)
	{
	case NameRole:					return colName;
	case TypeRole:					return "column";
	case ColumnTypeRole:			return int(colType);
	case ComputedColumnTypeRole:	return int(codeType);
	case IconSourceRole:			return JaspTheme::currentIconPath() + "/"+ getIconFilename(colType, varIconType::DefaultIconType);
	case ToolTipRole:
	{
		QString		usedIn	= colType == columnType::scale		? tr("which can be used in numerical comparisons and mathematical operations.")
							: colType == columnType::ordinal	? tr("which can only be used in (in)equivalence, greater and lesser than comparisons. Not in mathematical operations as subtraction etc, to do so: try converting to scalar first.")
																: tr("which can only be used in (in)equivalence comparisons. Not in greater/lesser-than comparisons or mathematical operations, to do so: try converting to ordinal or scalar first.");

		return tr("The '") + colName + tr("'-column ") + usedIn;
	}
	}
	
	return _tableModel->data(_tableModel->index(index.column(), index.row()), role);
}

int ColumnsModel::rowCount(const QModelIndex &) const
{
	return _tableModel->columnCount();
}

int ColumnsModel::columnCount(const QModelIndex &) const
{
	return 1;
}

QVariant ColumnsModel::provideInfo(varInfoType info, const QString& colName, int row) const
{
	ColumnsModel* colModel = ColumnsModel::singleton();

	if (!colModel)
		return QVariant();

	try
	{
		int colIndex = colName.isEmpty() ? 0 : colModel->getColumnIndex(fq(colName));

		if (colIndex < 0)
			return QVariant();

		return provideInfoAt(info, colIndex, row);
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return QVariant();
}

QVariant ColumnsModel::provideInfoAt(varInfoType info, int colIndex, int row) const
{
	JASPTIMER_SCOPE(ColumnsModel provideInfoAt);

	if (!ColumnsModel::singleton())
		return QVariant();

	try
	{
		QModelIndex qColIndex	= index(colIndex, 0),
					tableCIndex	= _tableModel->index(0, colIndex),
					tableVIndex	= _tableModel->index(row, colIndex);

		//columnType	colTypeHere	= static_cast<columnType>(colTypeInt);

		switch(info)
		{
		case varInfoType::VariableType:				return					data(qColIndex, ColumnsModel::ColumnTypeRole).toInt();
		case varInfoType::NameRole:					return					data(qColIndex, ColumnsModel::NameRole);
		
		case varInfoType::DoubleValues:				return	_tableModel->	data(tableCIndex,						int(dataPkgRoles::valuesDblList));
		case varInfoType::TotalNumericValues:		return	_tableModel->	data(tableCIndex,						int(dataPkgRoles::nonFilteredNumericValuesCount));
		case varInfoType::TotalLevels:				return	_tableModel->	data(tableCIndex,						int(dataPkgRoles::nonFilteredLevels)).toStringList().length();
		case varInfoType::Labels:					return	_tableModel->	data(tableCIndex,						int(dataPkgRoles::nonFilteredLevels));
		case varInfoType::DataSetValues:			return	_tableModel->	data(tableCIndex,						int(dataPkgRoles::valuesStrList));
		case varInfoType::DataSetRowCount:			return  _tableModel->	rowCount();
		case varInfoType::SignalsBlocked:			return	_tableModel->	synchingData();
		case varInfoType::DataSetValue:				return	_tableModel->	data(tableVIndex,						int(dataPkgRoles::value));
		
		case varInfoType::VariableNames:			return	getColumnNames();
		case varInfoType::DataAvailable:			return	MainWindow::singleton()->dataAvailable();
		
		case varInfoType::MaxWidth:					return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::maxColString)).toInt();
		case varInfoType::PreviewScale:				return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::previewScale));
		case varInfoType::PreviewOrdinal:			return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::previewOrdinal));
		case varInfoType::PreviewNominal:			return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::previewNominal));
		case varInfoType::ColumnDescription:		return	_tableModel->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::description));
		case varInfoType::DataSetPointer:			return	QVariant::fromValue<void*>(DataSetPackage::pkg()->dataSet());
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return QVariant();
}

bool ColumnsModel::absorbInfo(varInfoType info, const QString &colName, int row, QVariant value)
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
		case varInfoType::DataSetValue:					return	_tableModel->setData(qValIndex, value,	int(dataPkgRoles::value));
		case varInfoType::DataSetValues:				return	_tableModel->setData(qColIndex, value,	int(dataPkgRoles::valuesStrList));
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
		{ IconSourceRole,			"columnIcon"			},
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

void ColumnsModel::datasetChanged(  int										dataSetID,
									QStringList                             changedColumns,
									QStringList                             missingColumns,
									QMap<QString, QString>					changeNameColumns,
									bool                                    rowCountChanged,
									bool                                    hasNewColumns)
{
	//Only the shown dataset drives the visible column list (and the VariableInfo provider bound to it);
	//ignore column changes coming from background datasets.
	DataSet * shown = DataSetPackage::pkg()->dataSet();
	if(!shown || dataSetID != shown->id())
		return;

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

