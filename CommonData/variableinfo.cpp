#include "variableinfo.h"
#include "QQmlContext"
#include "dataset.h"
#include "column.h"
#include "QTimer"

VariableInfo::VariableInfo(VariableInfoProvider* providerInfo, QObject * parent) :
	QObject(parent ? parent : providerInfo && providerInfo->providerModel() ? providerInfo->providerModel() : nullptr)
{
	if(providerInfo)
		setProvider(providerInfo);
}

VariableInfo::~VariableInfo()
{
}

void VariableInfo::setProvider(VariableInfoProvider *provider)
{
	bool emitSome = _provider && _provider != provider;
	
	_provider = provider;
	
	if (!provider)
		return;
	
	connect(_provider->infoSignaller(),	&VarInfoSignaller::refresh,						this,	&VariableInfo::refresh				, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::filterChanged,				this,	&VariableInfo::filterChanged		, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::labelsChanged,				this,	&VariableInfo::labelsChanged		, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::dataSetChanged,				this,	&VariableInfo::dataSetChanged		, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::rowCountChanged,				this,	&VariableInfo::rowCountChanged		, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::labelsReordered,				this,	&VariableInfo::labelsReordered		, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::variablesChanged,			this,	&VariableInfo::variablesChanged		, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::variableTypeChanged,			this,	&VariableInfo::variableTypeChanged	, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::variableCountChanged,		this,	&VariableInfo::variableCountChanged	, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::dataAvailableChanged,		this,	&VariableInfo::dataAvailableChanged	, Qt::UniqueConnection);
	connect(_provider->infoSignaller(),	&VarInfoSignaller::variableNamesChanged,		this,	&VariableInfo::variableNamesChanged	, Qt::UniqueConnection);
	
	if(emitSome)
	{
		emit dataSetChanged();
		emit filterChanged();
		emit refresh();
	}
}

int VariableInfo::rowCount()
{
	return _provider ? _provider->provideInfo(varInfoType::DataSetRowCount).toInt() : 0;
}

int VariableInfo::variableCount()
{
	return _provider ? _provider->provideInfo(varInfoType::VariableNames).toStringList().count() : 0;
}

bool VariableInfo::dataAvailable()
{
	return _provider ? _provider->provideInfo(varInfoType::DataAvailable).toBool() : false;
}

DataSet *VariableInfo::dataSet()
{
	return _provider ? reinterpret_cast<DataSet*>(_provider->provideInfo(varInfoType::DataSetPointer).value<void*>()) : nullptr;
}

VariableInfoProvider::VariableInfoProvider(QObject *parent)
	: _infoSignaller(new VarInfoSignaller(parent))
{
	
}

void VarInfoSignaller::labelChanged(const Column * column, QString orgLabel, QString newLabel)
{
	emit labelsChanged(column->nameQ(), QMap{std::make_pair(orgLabel, newLabel)});
}
