#include "preferencesmodelbase.h"
#include "jasptheme.h"

PreferencesModelBase* PreferencesModelBase::_singleton = nullptr;

PreferencesModelBase::PreferencesModelBase(QObject *parent)
	: QObject{parent}
{
	assert(!_singleton);
	_singleton = this;
}

PreferencesModelBase::~PreferencesModelBase()
{
	assert(_singleton == nullptr || _singleton == this);
	_singleton = nullptr;
}

PreferencesModelBase *PreferencesModelBase::preferences()
{
	if (_singleton == nullptr)
		_singleton = new PreferencesModelBase();

	return _singleton;
}

void PreferencesModelBase::currentThemeNameHandler()
{
	setCurrentThemeName(JaspTheme::currentTheme()->themeName());
}
