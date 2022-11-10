#include "desktopcommunicator.h"

DesktopCommunicator::DesktopCommunicator(QObject *parent)
	: QObject{parent}
{
	assert(_singleton != nullptr);
	
	_singleton = this;
}

DesktopCommunicator * DesktopCommunicator::_singleton = nullptr;
