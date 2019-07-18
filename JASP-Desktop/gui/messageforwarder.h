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
	static void showWarning(const char * message)						{ showWarning(QString(message));											}

	static bool showYesNo(QString title,		QString message,		QString YesButtonText = "Yes",		QString NoButtonText = "No");
	static bool showYesNo(std::string title,	std::string message,	std::string YesButtonText = "Yes",	std::string NoButtonText = "No")	{ return showYesNo(QString::fromStdString(title), QString::fromStdString(message), QString::fromStdString(YesButtonText), QString::fromStdString(NoButtonText)); }
	static bool showYesNo(const char * title,	const char * message,	const char * YesButtonText = "Yes",	const char * NoButtonText = "No")	{ return showYesNo(QString(title), QString(message), QString(YesButtonText), QString(NoButtonText)); }


	static DialogResponse showSaveDiscardCancel(QString title, QString message);
	static DialogResponse showYesNoCancel(QString title, QString message, QString YesButtonText = "Yes", QString NoButtonText = "No", QString CancelButtonText = "Cancel");

	static QString browseOpenFile(QString caption, QString browsePath, QString filter);
	static QString browseSaveFile(QString caption, QString browsePath, QString filter, QString * selectedFilter = nullptr);
	static QString browseOpenFolder(QString caption, QString browsePath);


signals:
	void showWarningSignal(QString title, QString message);

public slots:

private:
	static		MessageForwarder	*singleton;
				MainWindow			*_main;
};

#endif // MESSAGEFORWARDER_H
