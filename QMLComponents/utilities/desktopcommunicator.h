#ifndef DESKTOPCOMMUNICATOR_H
#define DESKTOPCOMMUNICATOR_H

#include <QObject>
#include <condition_variable>
#include <mutex>
#include <QString>

///This class only exists to allow signal-slot connections to be made between certain classes in Desktop and in QMLComponents.
/// And to easily split that off for R-only
class DesktopCommunicator : public QObject
{
	Q_OBJECT
public:
	explicit DesktopCommunicator(QObject *parent = nullptr);
	
	static DesktopCommunicator * singleton();

	bool useNativeFileDialog();
	bool engineSandbox();
	char askCsvDelimiter(char autoDelimiter, const QString &data);
	bool queryEncryptionSettings(bool readingMode = false);
	char knownCsvDelimiter() const	{ return _knownCsvDelimiter; }
	void setKnownCsvDelimiter(char d)	{ _knownCsvDelimiter = d; }
	
signals:
	void queryEncryptionSettingsSignal(bool readingMode);
	void askCsvDelimiterSignal(const QString &data, char autoDelimiter);
	void currentJaspThemeChanged();
	void uiScaleChanged();
	void interfaceFontChanged();
	bool useNativeFileDialogSignal(); //< For internal use only, `bool useNativeFileDialog();` is what you want
	bool engineSandboxSignal();
	
public slots:
	void encryptionSettingsQueryComplete(bool submit);
	void delimiterChosen(char delimiter);
	
private:
	static DesktopCommunicator * _singleton;

	bool _queryCondition = false;
	bool _querySubmitted = false;
	bool _csvCondition = false;
	char _csvSubmitted = '\0';
	char _knownCsvDelimiter = '\0';

	std::mutex _queryLock;
	std::mutex _csvLock;

	std::condition_variable _query_cv;
	std::condition_variable _csv_cv;
};

#endif // DESKTOPCOMMUNICATOR_H
