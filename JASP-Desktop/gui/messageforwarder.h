#ifndef MESSAGEFORWARDER_H
#define MESSAGEFORWARDER_H

#include <QObject>
#include <string>

class MainWindow;

class MessageForwarder : public QObject
{
	Q_OBJECT
public:
	explicit MessageForwarder(MainWindow *main);

	static void showWarning(QString title, QString message);
	static void showWarning(std::string title, std::string message)		{ showWarning(QString::fromStdString(title),	QString::fromStdString(message));	}
	static void showWarning(const char * title, const char * message)	{ showWarning(QString(title),					QString(message));					}

	static void showWarning(QString message)							{ showWarning("",								message);					}
	static void showWarning(std::string message)						{ showWarning(QString::fromStdString(message));								}
	static void showWarning( const char * message)						{ showWarning(QString(message));											}

signals:
	void showWarningSignal(QString title, QString message);

public slots:

private:
	static MessageForwarder * singleton;
};

#endif // MESSAGEFORWARDER_H
