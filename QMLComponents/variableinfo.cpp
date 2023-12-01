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
	QString path = JaspTheme::currentIconPath();
	switch(type)
	{
	case VariableInfo::DefaultIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale.png";
		case columnType::ordinal:		return path + "variable-ordinal.png";
		case columnType::nominal:		return path + "variable-nominal.png";
		case columnType::nominalText:	return path + "variable-nominal-text.png";
		default:						return "";
		}
	case VariableInfo::DisabledIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale-disabled.png";
		case columnType::ordinal:		return path + "variable-ordinal-disabled.png";
		case columnType::nominal:		return path + "variable-nominal-disabled.png";
		case columnType::nominalText:	return path + "variable-nominal-text-inactive.png";
		default:						return "";
		}
	case VariableInfo::InactiveIconType:
		switch(colType)
		{
		case columnType::scale:			return path + "variable-scale-inactive.png";
		case columnType::ordinal:		return path + "variable-ordinal-inactive.png";
		case columnType::nominal:		return path + "variable-nominal-inactive.png";
		case columnType::nominalText:	return path + "variable-nominal-text-inactive.png";
		default:						return "";
		}
	}

	return ""; //We are never getting here but GCC isn't convinced
}

int VariableInfo::rowCount()
{
	return _provider ? _provider->provideInfo(VariableInfo::DataSetRowCount).toInt() : 0;
}

bool VariableInfo::dataAvailable()
{
	return _provider ? _provider->provideInfo(VariableInfo::DataAvailable).toBool() : false;
}
