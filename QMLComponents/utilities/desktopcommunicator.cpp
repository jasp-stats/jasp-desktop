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

bool DesktopCommunicator::queryEncryptionSettings(bool readingMode)
{
#ifdef BUILDING_JASP
	if(QThread::currentThread() == qApp->thread()) {
		int x = 1;

	}

	queryCondition = false;
	querySubmitted = false;
	std::unique_lock<std::mutex> lock(queryLock);
	emit queryEncryptionSettingsSignal(readingMode);
	query_cv.wait(lock, [&] { return queryCondition; });

	return querySubmitted;
#else
	return true;
#endif
}

void DesktopCommunicator::encryptionSettingsQueryComplete(bool submit)
{
	std::lock_guard<std::mutex> lock(queryLock);
	queryCondition = true;
	querySubmitted = submit;
	query_cv.notify_one();
}
