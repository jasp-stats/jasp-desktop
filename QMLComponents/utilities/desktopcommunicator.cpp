#include "desktopcommunicator.h"

DesktopCommunicator::DesktopCommunicator(QObject *parent)
	: QObject{parent}
{
	assert(_singleton != nullptr);
	
	_singleton = this;
}

bool DesktopCommunicator::useNativeFileDialog()
{
#ifdef BUILDING_JASP
	return emit useNativeFileDialogSignal();
#else
	return true;
#endif
}

DesktopCommunicator * DesktopCommunicator::_singleton = nullptr;
