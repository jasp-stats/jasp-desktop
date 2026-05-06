#include "desktopcommunicator.h"
#include <QThread>
#include <QGuiApplication>
#include <cassert>
#include <QMetaMethod>

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

	_queryCondition = false;
	_querySubmitted = false;
	std::unique_lock<std::mutex> lock(_queryLock);
	emit queryEncryptionSettingsSignal(readingMode);
	_query_cv.wait(lock, [&] { return _queryCondition; });

	return _querySubmitted;
#else
	return true;
#endif
}

char DesktopCommunicator::askCsvDelimiter(char autoDelimiter, const QString &data)
{
#ifdef BUILDING_JASP
	if (_knownCsvDelimiter != '\0')
		return _knownCsvDelimiter;

	if(QThread::currentThread() == qApp->thread()) {
		int x = 1;
	}

	_csvCondition = false;
	_csvSubmitted = autoDelimiter;
	
	if(!QObject::isSignalConnected(QMetaMethod::fromSignal(&DesktopCommunicator::askCsvDelimiterSignal)))
		return autoDelimiter;
	
	emit askCsvDelimiterSignal(data, autoDelimiter);

	std::unique_lock<std::mutex> lock(_csvLock);
	_csv_cv.wait(lock, [&] { return _csvCondition; });

	return _csvSubmitted;
#else
	return ',';
#endif
}

void DesktopCommunicator::encryptionSettingsQueryComplete(bool submit)
{
	std::lock_guard<std::mutex> lock(_queryLock);
	_queryCondition = true;
	_querySubmitted = submit;
	_query_cv.notify_one();
}

void DesktopCommunicator::delimiterChosen(char delimiter)
{
	std::lock_guard<std::mutex> lock(_csvLock);
	_csvCondition = true;
	_csvSubmitted = delimiter;
	_csv_cv.notify_one();
}
