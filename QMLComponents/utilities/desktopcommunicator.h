#ifndef DESKTOPCOMMUNICATOR_H
#define DESKTOPCOMMUNICATOR_H

#include <QObject>

///This class only exists to allow signal-slot connections to be made between certain classes in Desktop and in QMLComponents.
/// And to easily split that off when building for R -only
class DesktopCommunicator : public QObject
{
	Q_OBJECT
public:
	explicit DesktopCommunicator(QObject *parent = nullptr);
	
	static DesktopCommunicator * singleton() { return _singleton; }

signals:
	void currentJaspThemeChanged();
	
private:
	static DesktopCommunicator * _singleton;
};

#endif // DESKTOPCOMMUNICATOR_H
