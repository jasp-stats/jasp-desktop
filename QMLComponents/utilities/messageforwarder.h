#ifndef MESSAGEFORWARDER_H
#define MESSAGEFORWARDER_H

#include <QQuickItem>
#include <qmessagebox.h>
#include <string>

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


	explicit MessageForwarder(QObject *main);
	~MessageForwarder() { _singleton = nullptr;}

	static MessageForwarder * msgForwarder() { return _singleton; }

	static QMessageBox* getInfoBox(const QString& title, const QString& message);

	static void showWarning(QString title, QString message,				QMessageBox::Icon icon = QMessageBox::Warning);
	static void showWarning(std::string title, std::string message,		QMessageBox::Icon icon = QMessageBox::Warning)		{ showWarning(QString::fromStdString(title),	QString::fromStdString(message),	icon);	}
	static void showWarning(const char * title, const char * message,	QMessageBox::Icon icon = QMessageBox::Warning)		{ showWarning(QString(title),					QString(message),					icon);	}

	static void showWarning(QString message,							QMessageBox::Icon icon = QMessageBox::Warning)		{ showWarning("",								message,							icon);	}
	static void showWarning(std::string message,						QMessageBox::Icon icon = QMessageBox::Warning)		{ showWarning(QString::fromStdString(message),										icon);	}
	static void showWarning(const char * message,						QMessageBox::Icon icon = QMessageBox::Warning)		{ showWarning(QString(message),														icon);	}

	static bool showYesNo(QString title,		QString message,		QString YesButtonText		= "",	QString NoButtonText = "",			QMessageBox::Icon icon = QMessageBox::Question);
	static bool showYesNo(std::string title,	std::string message,	std::string YesButtonText	= "",	std::string NoButtonText = "",		QMessageBox::Icon icon = QMessageBox::Question)	{ return showYesNo(QString::fromStdString(title), QString::fromStdString(message), QString::fromStdString(YesButtonText), QString::fromStdString(NoButtonText), icon); }
	static bool showYesNo(const char * title,	const char * message,	const char * YesButtonText	= "",	const char * NoButtonText = "",		QMessageBox::Icon icon = QMessageBox::Question)	{ return showYesNo(QString(title), QString(message), QString(YesButtonText), QString(NoButtonText), icon); }


	static DialogResponse showSaveDiscardCancel(QString title, QString message, QString saveTxt = "",		QString discardText = "",	QString cancelText = "");
	static DialogResponse showYesNoCancel(		QString title, QString message, QString YesButtonText = "", QString NoButtonText = "",	QString CancelButtonText = "",	QMessageBox::Icon icon = QMessageBox::Question);

	static QString browseOpenFile(			QString caption, QString browsePath,	QString filter, bool multiple = false);
    static QString browseSaveFile(			QString caption, QString browsePath,	QString filter, QString * selectedExtension = nullptr);
    static QString browseSaveFile(const QString& caption, const QString& browsePath,	const QString& filter, QString& selectedFilter, QString & selectedExtension);
    static QString browseOpenFolder(		QString caption, QString browsePath);
	static QString browseOpenFolder(		QString caption);
	static QString browseOpenFileDocuments(	QString caption,						QString filter, bool multiple = false);
	static QString browseSaveFileDocuments(	QString caption,						QString filter);
    static QString queryTextInput(const QString& caption, const QString& elementName, const QString& defaultValue, bool& passwordGiven, bool password = true);

	static QString askPassword(QString title, QString message);

	//Some non-static links to have QML handle it. Without figuring out how qmlRegisterSingletonType() works :p
public slots:
	bool		showYesNoQML(QString title, QString message, QString yesText = "", QString noText = "")	{ return showYesNo(title, message, yesText, noText); }
	DialogResponse	showSaveDiscardCancelQML(QString title, QString message, QString saveTxt = "", QString discardText = "",	QString cancelText = "")	{ return showSaveDiscardCancel(title, message, saveTxt, discardText, cancelText); }
	void			showWarningQML(QString title, QString message)																							{ showWarning(title, message); }
	QString			browseOpenFileQML(QString caption, QString browsePath, QString filter, bool multiple = false)					{ return constrainToSandboxResult(browseOpenFile(caption, constrainToSandboxStartDir(browsePath), filter, multiple)); }
	QString			browseOpenFileDocumentsQML(QString caption, QString filter, bool multiple = false)								{ return constrainToSandboxResult(browseOpenFileDocuments(caption, filter, multiple)); }

	QString			browseSaveFileQML(QString caption, QString browsePath, QString filter)											{ return constrainToSandboxResult(browseSaveFile(caption, constrainToSandboxStartDir(browsePath), filter), true, true); }
	QString			browseSaveFileDocumentsQML(QString caption, QString filter)														{ return constrainToSandboxResult(browseSaveFileDocuments(caption, filter), true, true); }
	QString			browseOpenFolderQML(QString caption, QString browsePath)														{ return constrainToSandboxResult(browseOpenFolder(caption, constrainToSandboxStartDir(browsePath)), false);}
	QString			browseOpenFolderQML(QString caption)																			{ return constrainToSandboxResult(browseOpenFolder(caption), false);}

	void			log(QString msg);

private:
	static		bool				useNativeFileDialogs();
	static		bool				engineSandbox();
	static		MessageForwarder	* _singleton;

	//these functions constrain the in/out folder/file to the sandbox
	static		QString				constrainToSandboxResult(const QString& selectedPath, bool file = true, bool save = false);
	static		QString				constrainToSandboxStartDir(const QString& initialPath);
};

#endif // MESSAGEFORWARDER_H
