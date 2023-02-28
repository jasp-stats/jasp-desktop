#include "changesetvalue.h"
#include "utilities/qutils.h"


namespace Modules
{

ChangeSetValue::ChangeSetValue()
{

}

void ChangeSetValue::applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const
{
	const std::string name = fq(_name);

	options[name]	= _jsonValue;

	msgs[logId].push_back(prefixLog + "Set value '" + _jsonValue.toStyledString()  + "' on option '" + name + "'.");

	if(msg() != "")
		msgs[name].push_back(fq(msg()));
}

QString ChangeSetValue::toString() const	
{
	return _toString() + " setting value to option '" + name() + "', value: '" + tq(_jsonValue.toStyledString()) + "'";
}

void ChangeSetValue::setName(QString name)
{
	if (_name == name)
		return;
	
	_name = name;
	emit nameChanged(_name);
}

QJSValue ChangeSetValue::jsonValue() const
{
	return tqj(_jsonValue, this);
}

void ChangeSetValue::setJsonValue(QJSValue jsonValue)
{
	Json::Value json = fqj(jsonValue);

	if (_jsonValue == json)
		return;

	_jsonValue = json;
	emit jsonValueChanged();
}

}
