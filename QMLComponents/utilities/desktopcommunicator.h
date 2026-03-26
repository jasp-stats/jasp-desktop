#ifndef DESKTOPCOMMUNICATOR_H
#define DESKTOPCOMMUNICATOR_H

#include <QObject>
#include <condition_variable>
#include <mutex>

///This class only exists to allow signal-slot connections to be made between certain classes in Desktop and in QMLComponents.
/// And to easily split that off when building for R -only
class DesktopCommunicator : public QObject
{
	Q_OBJECT
public:
	explicit DesktopCommunicator(QObject *parent = nullptr);
	
	static DesktopCommunicator * singleton();

	bool useNativeFileDialog();
	bool engineSandbox();
	bool queryEncryptionSettings(bool readingMode = false);

signals:
	void queryEncryptionSettingsSignal(bool readingMode);
	void currentJaspThemeChanged();
	void uiScaleChanged();
	void interfaceFontChanged();
	bool useNativeFileDialogSignal(); //< For internal use only, `bool useNativeFileDialog();` is what you want
	bool engineSandboxSignal();

public slots:
	void encryptionSettingsQueryComplete(bool submit);

private:
	static DesktopCommunicator * _singleton;

	bool queryCondition = false; // against spurious wakeup
	bool querySubmitted = false;
	std::mutex queryLock;
	std::condition_variable query_cv;
};

#endif // DESKTOPCOMMUNICATOR_H
