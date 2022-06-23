#include "preferencesmodelbase.h"
#include "qquick/jasptheme.h"

PreferencesModelBase* PreferencesModelBase::_singleton = nullptr;

PreferencesModelBase::PreferencesModelBase(QObject *parent)
	: QObject{parent}
{
	_singleton = this;
}

PreferencesModelBase *PreferencesModelBase::prefs()
{
	if (_singleton == nullptr)
		_singleton = new PreferencesModelBase();

	return _singleton;
}

void PreferencesModelBase::currentThemeNameHandler()
{
	setCurrentThemeName(JaspTheme::currentTheme()->themeName());
}

