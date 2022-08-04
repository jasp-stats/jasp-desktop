#include "changejs.h"
#include "utilities/qutils.h"


namespace Modules
{

ChangeJS::ChangeJS()
{

}

void ChangeJS::applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const
{
	const std::string name = fq(_name);

	if(!_jsFunction.isCallable())
		throw upgradeError("Could not apply ChangeJS to option '" + name + "' because the function cannot be called...");

	if(!options.isMember(name))
		throw upgradeError("Could not apply ChangeJS for option '" + name + "' because options does not contain '" + name + "'");

	QJSValue	copyFunc	= _jsFunction, //To avoid const clashing with javascript "flexibility"
				result		= copyFunc.call( { tqj(options, this) } );

	if(result.isError())
		throw upgradeError("Could not apply ChangeJS for option '" + name + "' because the javascript function returned an error: '" + fq(result.toString()) + "'");

	options[name] = fqj(result);

	msgs[logId].push_back(prefixLog + "Applied ChangeJS to option '" + name + "', result was: '" + options[name].toStyledString() + "'");

	if(msg() != "")
		msgs[name].push_back(fq(msg()));
}

void ChangeJS::setName(QString name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged(_name);
}

void ChangeJS::setJsFunction(QJSValue jsFunction)
{
	_jsFunction = jsFunction;
	emit jsFunctionChanged();
}

}
