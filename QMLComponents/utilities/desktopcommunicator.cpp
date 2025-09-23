#include "desktopcommunicator.h"
#include <QThread>
#include <QGuiApplication>

DesktopCommunicator * DesktopCommunicator::_singleton = nullptr;

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

void DesktopCommunicator::queryEncryptionSettings()
{
#ifdef BUILDING_JASP
	if(QThread::currentThread() == qApp->thread()) {
		int x = 1;

	}

	queryCondition = false;
	std::unique_lock<std::mutex> lock(queryLock);
	emit queryEncryptionSettingsSignal();
	query_cv.wait(lock, [&] { return queryCondition; });
#else
	return;
#endif
}

void DesktopCommunicator::encryptionSettingsQueryComplete()
{
	std::lock_guard<std::mutex> lock(queryLock);
	queryCondition = true;
	query_cv.notify_one();
}
