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

	enum class DialogResponse { Cancel, Yes, No, Save, Discard };

	static void showWarning(QString title, QString message);
	static void showWarning(std::string title, std::string message)		{ showWarning(QString::fromStdString(title),	QString::fromStdString(message));	}
	static void showWarning(const char * title, const char * message)	{ showWarning(QString(title),					QString(message));					}

	static void showWarning(QString message)							{ showWarning("",								message);					}
	static void showWarning(std::string message)						{ showWarning(QString::fromStdString(message));								}
	static void showWarning( const char * message)						{ showWarning(QString(message));											}

	static bool showYesNo(QString title, QString message);

	static DialogResponse showSaveDiscardCancel(QString title, QString message);
	static DialogResponse showYesNoCancel(QString title, QString message, QString YesButtonText = "Yes", QString NoButtonText = "No", QString CancelButtonText = "Cancel");

	static QString openFileBrowse(QString caption, QString browsePath, QString filter);
	static QString saveFileBrowse(QString caption, QString browsePath, QString filter, QString * selectedFilter = nullptr);


signals:
	void showWarningSignal(QString title, QString message);

public slots:

private:
	static		MessageForwarder	*singleton;
				MainWindow			*_main;
};

#endif // MESSAGEFORWARDER_H
