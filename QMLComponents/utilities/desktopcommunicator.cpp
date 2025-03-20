#include "desktopcommunicator.h"

DesktopCommunicator::DesktopCommunicator(QObject *parent)
	: QObject{parent}
{
	assert(!_singleton);
	
	_singleton = this;
}

DesktopCommunicator *DesktopCommunicator::singleton()
{
	if(!_singleton)
		new DesktopCommunicator();

	return _singleton;
}

bool DesktopCommunicator::useNativeFileDialog()
{
#ifdef BUILDING_JASP
	return emit useNativeFileDialogSignal();
#else
	return true;
#endif
}

bool DesktopCommunicator::engineSandbox()
{
#ifdef BUILDING_JASP
	return emit engineSandboxSignal();
#else
	return false;
#endif
}

DesktopCommunicator * DesktopCommunicator::_singleton = nullptr;
