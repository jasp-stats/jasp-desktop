#include "log.h"
#include "utilities/qutils.h"
#include "../analysisentry.h"
#include "gui/preferencesmodel.h"

#define DELAYED_ENUM_DECLARATION_CPP
#include "entrybase.h"

namespace Modules
{

EntryError::EntryError(QString problem)	: std::runtime_error("Entry specification for JASP Module has a problem: " + fq(problem)) {}

const char * EntryError::what() const noexcept { return std::runtime_error::what(); }

EntryBase::EntryBase(EntryType entryType) : DescriptionChildBase(), _entryType(entryType)
{
	connect(PreferencesModel::prefs(), &PreferencesModel::developerModeChanged, this, &EntryBase::devModeChanged);
}

void EntryBase::devModeChanged(bool)
{
	if(debug()) emit somethingChanged(this);
}

QString EntryBase::toString() const
{
	switch(_entryType)
	{
	case EntryType::separator:	return "-- separator --";
	case EntryType::groupTitle:	return	"-- groupTitle '" + title() + "' icon: '" + icon() + "' --";
	case EntryType::analysis:
		return	"-- analysis '"		+ title()
				+ ( menu() != title() ? " menu: '" + menu() + "'" : "" )
				+ ", function: '"	+ function() + "'"
				+ ", qml: '"		+ qml() + "'"
				+ ", reqData: "		+ (requiresData() ? "yes":"no")
				+ ", icon: '"		+ icon() + "' --";
	}

	return "???";
}

bool EntryBase::shouldBeAdded() const
{
	return enabled() && (!debug() || PreferencesModel::prefs()->developerMode());
}

void EntryBase::setMenu(QString menu)
{
	if(_entryType != EntryType::analysis)
		throw EntryError("You cannot set 'menu' of an entry that is not of type 'analysis'.");

	if (_menu == menu)
		return;

	_menu = menu;
	emit menuChanged(_menu);
}

void EntryBase::setTitle(QString title)
{
	if(_entryType == EntryType::separator)
		throw EntryError("You cannot set a title on a separator.");

	if (_title == title)
		return;

	_title = title;
	emit titleChanged(_title);
}

void EntryBase::setFunction(QString function)
{
	if(_entryType != EntryType::analysis)
		throw EntryError("You cannot set 'func(tion)' of an entry that is not of type 'analysis'.");

	if (_function == function)
		return;

	_function = function;
	emit functionChanged(_function);
}

void EntryBase::setIcon(QString icon)
{
	if(_entryType == EntryType::separator)
		throw EntryError("You cannot set an icon on a separator.");

	if (_icon == icon)
		return;

	_icon = icon;
	emit iconChanged(_icon);
}

void EntryBase::setRequiresData(bool requiresData)
{
	_useDefaultRequiresData = false;

	if (_requiresData == requiresData)
		return;

	_requiresData = requiresData;
	emit requiresDataChanged(_requiresData);
}

void EntryBase::setEnabled(bool enabled)
{
	if (_enabled == enabled)
		return;

	_enabled = enabled;
	emit enabledChanged(_enabled);
}

void EntryBase::setQml(QString qml)
{
	if(_entryType != EntryType::analysis)
		throw EntryError("You cannot set 'qml' of an entry that is not of type 'analysis'.");

	if (_qml == qml)
		return;

	_qml = qml;
	emit qmlChanged(_qml);
}

void EntryBase::setDebug(bool debug)
{
	if (_debug == debug)
		return;

	_debug = debug;
	emit debugChanged(_debug);
}

AnalysisEntry * EntryBase::convertToAnalysisEntry(bool requiresDataDefault) const
{
	AnalysisEntry * entry = new AnalysisEntry();

	entry->_qml				= qml()  != "" ? fq(qml())  : fq(function() + ".qml") ;
	entry->_menu			= menu() != "" ? fq(menu()) : fq(title());
	entry->_icon			= fq(icon());
	entry->_title			= fq(title());
	entry->_function		= fq(function());
	entry->_requiresData	= _useDefaultRequiresData ? requiresDataDefault : requiresData();

	entry->_isEnabled		= _enabled;
	entry->_isAnalysis		= _entryType == EntryType::analysis;
	entry->_isSeparator		= _entryType == EntryType::separator;
	entry->_isGroupTitle	= _entryType == EntryType::groupTitle;

	entry->_dynamicModule	= _description->dynMod();

	return entry;
}

}
