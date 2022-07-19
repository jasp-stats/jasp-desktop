#ifndef MESSAGEFORWARDER_H
#define MESSAGEFORWARDER_H

#include <QQuickItem>
#include <string>

class MainWindow;

///
/// Singleton class for shwoing warnings, messages, etc
/// Can be accessed through the static function anywhere in Desktop
/// Can also be accessed through `messages.` in QML
class MessageForwarder : public QQuickItem
{
	Q_OBJECT
public:
	enum class DialogResponse { Cancel, Yes, No, Save, Discard };
	Q_ENUM(DialogResponse)


	explicit MessageForwarder(MainWindow *main);
	~MessageForwarder() { _singleton = nullptr;}

	static MessageForwarder * msgForwarder() { return _singleton; }


	static void showWarning(QString title, QString message);
	static void showWarning(std::string title, std::string message)		{ showWarning(QString::fromStdString(title),	QString::fromStdString(message));	}
	static void showWarning(const char * title, const char * message)	{ showWarning(QString(title),					QString(message));					}

	static void showWarning(QString message)							{ showWarning("",								message);					}
	static void showWarning(std::string message)						{ showWarning(QString::fromStdString(message));								}
	static void showWarning(const char * message)						{ showWarning(QString(message));											}

	static bool showYesNo(QString title,		QString message,		QString YesButtonText		= "",	QString NoButtonText = "");
	static bool showYesNo(std::string title,	std::string message,	std::string YesButtonText	= "",	std::string NoButtonText = "")	{ return showYesNo(QString::fromStdString(title), QString::fromStdString(message), QString::fromStdString(YesButtonText), QString::fromStdString(NoButtonText)); }
	static bool showYesNo(const char * title,	const char * message,	const char * YesButtonText	= "",	const char * NoButtonText = "")	{ return showYesNo(QString(title), QString(message), QString(YesButtonText), QString(NoButtonText)); }


	static DialogResponse showSaveDiscardCancel(QString title, QString message, QString saveTxt = "",		QString discardText = "",	QString cancelText = "");
	static DialogResponse showYesNoCancel(		QString title, QString message, QString YesButtonText = "", QString NoButtonText = "",	QString CancelButtonText = "");

	static QString browseOpenFile(			QString caption, QString browsePath,	QString filter);
	static QString browseSaveFile(			QString caption, QString browsePath,	QString filter, QString * selectedExtension = nullptr);
	static QString browseOpenFolder(		QString caption, QString browsePath);
	static QString browseOpenFileDocuments(	QString caption,						QString filter);
	static QString browseSaveFileDocuments(	QString caption,						QString filter);

	static QString askPassword(QString message);

	//Some non-static links to have QML handle it. Without figuring out how qmlRegisterSingletonType() works :p
public slots:
	DialogResponse	showSaveDiscardCancelQML(QString title, QString message, QString saveTxt = "", QString discardText = "",	QString cancelText = "")	{ return showSaveDiscardCancel(title, message, saveTxt, discardText, cancelText); }
	void			showWarningQML(QString title, QString message)																							{ showWarning(title, message); }
	QString			browseOpenFileQML(QString caption, QString browsePath, QString filter)																	{ return browseOpenFile(caption, browsePath, filter); }
	QString			browseOpenFileDocumentsQML(QString caption, QString filter)																				{ return browseOpenFileDocuments(caption, filter); }

	QString			browseSaveFileQML(QString caption, QString browsePath, QString filter)																	{ return browseSaveFile(caption, browsePath, filter); }
	QString			browseSaveFileDocumentsQML(QString caption, QString filter)																				{ return browseSaveFileDocuments(caption, filter); }

private:
	static		MessageForwarder	*_singleton;
				MainWindow			*_main;
};

#endif // MESSAGEFORWARDER_H
