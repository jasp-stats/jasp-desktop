#include "variableinfo.h"
#include "jasptheme.h"
#include "QQmlContext"
#include "QTimer"

VariableInfo* VariableInfo::_singleton = nullptr;

VariableInfo::VariableInfo(VariableInfoProvider* providerInfo) :
	QObject(providerInfo->providerModel()), _provider(providerInfo)
{
	if (_singleton == nullptr)
	{
		_singleton = this;
		QTimer::singleShot(0, [&]() { _setDataSetInfoInContext(); });
	}
}

void VariableInfo::_setDataSetInfoInContext()
{
	QQmlContext* context = _provider->providerQMLContext();
	context->setContextProperty("dataSetInfo", this);
}

VariableInfo *VariableInfo::info()
{
	return _singleton;
}

QString VariableInfo::getIconFile(columnType colType, VariableInfo::IconType type)
{
	QString iconType;
	
	switch(type)
	{
	default:
	case VariableInfo::DefaultIconType:		iconType = "";				break;
	case VariableInfo::DisabledIconType:	iconType = "-disabled";		break;
	case VariableInfo::InactiveIconType:	iconType = "-inactive";		break;		
	case VariableInfo::TransparentIconType:	iconType = "-transparent";	break;
	}
	
	return QString("%1variable-%2%3.svg").arg(JaspTheme::currentIconPath()).arg(columnTypeToQString(colType)).arg(iconType);
}

int VariableInfo::rowCount()
{
	return _provider ? _provider->provideInfo(VariableInfo::DataSetRowCount).toInt() : 0;
}

bool VariableInfo::dataAvailable()
{
	return _provider ? _provider->provideInfo(VariableInfo::DataAvailable).toBool() : false;
}
