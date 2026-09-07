#include "messageforwarder.h"
#include "utilities/desktopcommunicator.h"
#include <QMessageBox>
#include <QPushButton>
#include "databaseconnectioninfo.h"
#include <QRegularExpression>
#include <QInputDialog>
#include <QFileDialog>
#include <QString>
#include "qutils.h"
#include "appdirs.h"
#include "log.h"

MessageForwarder::MessageForwarder(QObject *main) : QQuickItem(nullptr)
{
	if(_singleton != nullptr)
		throw std::runtime_error("There can be only ONE MessageForwarder!");

	_singleton = this;

	setParent(main);
}

void MessageForwarder::log(QString msg)
{
	Log::log() << msg << std::endl;
}

void MessageForwarder::linkWithDatabaseConnectionInfo(DatabaseConnectionInfo *info)
{
	connect(info,		&DatabaseConnectionInfo::showYesNo,			_singleton,		&MessageForwarder::showYesNoSlot			);
	connect(info,		&DatabaseConnectionInfo::askPassword,		_singleton,		&MessageForwarder::askPasswordSlot			);
	connect(info,		&DatabaseConnectionInfo::showWarning,		_singleton,		&MessageForwarder::showWarningQML			);
}

bool MessageForwarder::useNativeFileDialogs()
{
	return DesktopCommunicator::singleton()->useNativeFileDialog();
}

bool MessageForwarder::engineSandbox()
{
	return DesktopCommunicator::singleton()->engineSandbox();
}

QString MessageForwarder::constrainToSandboxResult(const QString &selectedPath, bool file, bool save)
{
	if(!engineSandbox() || selectedPath == "")
		return selectedPath;

	QString agg = "";
	auto sandbox = QDir(AppDirs::sandboxedDocuments()).filesystemAbsolutePath();
	for(auto& path : selectedPath.split(";")) {
		QFileInfo info(path);
		auto fileName = info.fileName();
		auto dir = info.dir();

		QString res = path;
		auto target = dir.filesystemAbsolutePath();
		if(!std::filesystem::equivalent(sandbox, target)) {
			if(!file) {
				res = AppDirs::sandboxedDocuments();
			}
			else {
				res = QDir(AppDirs::sandboxedDocuments()).filePath(fileName);
			}

			if(save && file)
				showWarning(tr("Sandbox warning"), tr("You want to save a file outside of the sandbox directory! We will correct the path to: ") + res);
			else if(!save && file)
				showWarning(tr("Sandbox warning"), tr("You want to load a file outside of the sandbox directory! This will fail please move the file to: ") + res);
			else
				showWarning(tr("Sandbox warning"), tr("The jasp engine is running in security sandbox mode and only has access to: ") + res);
		}
		agg += res + ";";
	}
	agg.chop(1);

	return agg;
}

QString MessageForwarder::constrainToSandboxStartDir(const QString &initialPath)
{
	if(!engineSandbox())
		return initialPath;

	return AppDirs::sandboxedDocuments();
}

MessageForwarder * MessageForwarder::_singleton = nullptr;

QMessageBox *MessageForwarder::getInfoBox(const QString &title, const QString &message)
{
	QMessageBox *msgBox = new QMessageBox(nullptr);
	msgBox->setIcon( QMessageBox::Information );
	msgBox->setWindowTitle(title);
	msgBox->setText(message);
	QPushButton *btn =  msgBox->addButton( "OK", QMessageBox::AcceptRole );
	msgBox->setAttribute(Qt::WA_DeleteOnClose); // delete pointer after close
	msgBox->setModal(false);
	return msgBox;
}

void MessageForwarder::showWarning(QString title, QString message, QMessageBox::Icon icon)
{
	//A modal dialog would block an automated test runner forever, so in tests it becomes a log line.
	//Same pattern as MainWindow::checkForUpdates, which also stays out of the way in tests.
	if(QCoreApplication::applicationName() == "JASPTest")
	{
		Log::log() << "[MessageForwarder::showWarning] suppressed in test: " << fq(message) << std::endl;
		return;
	}

	QMessageBox box;
	box.setText(title);
	box.setInformativeText(message);
	box.setIcon(icon);

	box.exec();
}

bool MessageForwarder::showYesNo(QString title, QString message, QString YesButtonText, QString NoButtonText, QMessageBox::Icon icon)
{
	if(YesButtonText == "")		YesButtonText	= tr("Yes");
	if(NoButtonText == "")		NoButtonText	= tr("No");

	QMessageBox box;

	box.setText(title);
	box.setInformativeText(message);
	box.setIcon(QMessageBox::Question);

	QPushButton* yesButton =	box.addButton(YesButtonText,	QMessageBox::ButtonRole::YesRole);
	QPushButton* noButton =		box.addButton(NoButtonText,		QMessageBox::ButtonRole::NoRole);

	box.setDefaultButton(yesButton);
	box.exec();

	return box.clickedButton() == yesButton;
}

MessageForwarder::DialogResponse MessageForwarder::showYesNoCancel(QString title, QString message, QString YesButtonText, QString NoButtonText, QString CancelButtonText, QMessageBox::Icon icon)
{
	if(YesButtonText == "")		YesButtonText		= tr("Yes");
	if(NoButtonText == "")		NoButtonText		= tr("No");
	if(CancelButtonText == "")	CancelButtonText	= tr("Cancel");

	QMessageBox box;

	box.setText(title);
	box.setInformativeText(message);
	box.setIcon(icon);

	QPushButton* yesButton =	box.addButton(YesButtonText,		QMessageBox::ButtonRole::YesRole);
	QPushButton* noButton =		box.addButton(NoButtonText,			QMessageBox::ButtonRole::NoRole);
	QPushButton* cancelButton = box.addButton(CancelButtonText,		QMessageBox::ButtonRole::RejectRole);
	box.setDefaultButton(cancelButton);

	box.exec();

	QAbstractButton * clicked = box.clickedButton();
	if		(clicked == yesButton)	return DialogResponse::Yes;
	else if (clicked == noButton)	return DialogResponse::No;

	return DialogResponse::Cancel;
}

MessageForwarder::DialogResponse MessageForwarder::showSaveDiscardCancel(QString title, QString message, QString saveText, QString discardText, QString cancelText)
{
	QMessageBox box;

	box.setText(title);
	box.setInformativeText(message);
	box.setIcon(QMessageBox::Question);

	if(saveText == "")		saveText	= tr("Save");
	if(discardText == "")	discardText = tr("Don't Save");
	if(cancelText == "")	cancelText	= tr("Cancel");

	// In order to have the noSaveButton as first in the row of buttons, it has to get the role RejectRole.
	QPushButton* saveButton =	box.addButton(saveText,		QMessageBox::ButtonRole::AcceptRole);
	QPushButton* noSaveButton =	box.addButton(discardText,	QMessageBox::ButtonRole::DestructiveRole);
								box.addButton(cancelText,	QMessageBox::ButtonRole::RejectRole);

	box.setDefaultButton(saveButton);
	box.exec();

	QAbstractButton* clicked = box.clickedButton();

	if		(clicked == saveButton)		return DialogResponse::Save;
	else if (clicked == noSaveButton)	return DialogResponse::Discard;

	return DialogResponse::Cancel;
}

QString MessageForwarder::askPassword(QString title, QString message)
{
//	here we can open a nice QInputDialog with a password field etc (modally)
	return QInputDialog::getText(nullptr, title, message, QLineEdit::Password);
}

QString MessageForwarder::browseOpenFile(QString caption, QString browsePath, QString filter, bool multiple)
{
	QFileDialog::Options options = useNativeFileDialogs() ? QFileDialog::Options() : QFileDialog::DontUseNativeDialog;

	if (multiple)	return QFileDialog::getOpenFileNames(nullptr, caption, browsePath, filter, nullptr, options).join(';');
	else			return QFileDialog::getOpenFileName(nullptr, caption, browsePath, filter, nullptr, options);
}

QString MessageForwarder::browseOpenFileDocuments(QString caption, QString filter, bool multiple)
{
	return browseOpenFile(caption, constrainToSandboxStartDir(AppDirs::documents()), filter, multiple);
}

QString MessageForwarder::browseSaveFileDocuments(QString caption, QString filter)
{
	return browseSaveFile(caption, constrainToSandboxStartDir(AppDirs::documents()), filter);
}

QString MessageForwarder::queryTextInput(const QString &caption, const QString &elementName, const QString &defaultValue, bool& passwordGiven, bool password)
{
    bool ok{};
    QString text = QInputDialog::getText(nullptr, caption,
                                         elementName, password ? QLineEdit::Password : QLineEdit::Normal,
                                         defaultValue, &passwordGiven);
    if (!passwordGiven)  text = "";
    return text;
}

QString MessageForwarder::browseSaveFile(const QString& caption, const QString& browsePath, const QString& filter, QString& selectedFilter, QString & selectedExtension) {
    Log::log() << "MessageForwarder::browseSaveFile(\"" << caption.toStdString() << "\", \"" << browsePath.toStdString() << "\", \"" << filter.toStdString() << "\")" << std::endl;

    QString saveFileName;

    if(useNativeFileDialogs())	saveFileName = 	QFileDialog::getSaveFileName(nullptr, caption, browsePath, filter, &selectedFilter);
    else						saveFileName = 	QFileDialog::getSaveFileName(nullptr, caption, browsePath, filter, &selectedFilter, QFileDialog::DontUseNativeDialog);

    Log::log() << "Selected save file: " << saveFileName << " and selected filter: " << selectedFilter << std::endl;

    //Lets make sure the extension is added:
    static const QRegularExpression extReg("\\*\\.(\\w+)");
    QRegularExpressionMatch  possibleMatch = extReg.match(selectedFilter);

    if(possibleMatch.hasMatch())
    {
        QString ext = possibleMatch.captured(1);

        if(!saveFileName.endsWith(ext))
            saveFileName += "." + ext;

        selectedExtension = ext;
    }
    else //So the filter doesnt tell us the extension but the caller expects to know what is what
    {
        if(saveFileName.lastIndexOf('.') >= 0) selectedExtension = saveFileName.mid(saveFileName.lastIndexOf('.') + 1);
        else	 selectedExtension = ""; //???
    }

    Log::log() << "Selected extension: '" << selectedExtension << "'" << std::endl;

    return saveFileName;
}


QString MessageForwarder::browseSaveFile(QString caption, QString browsePath, QString filter, QString * selectedExtension)
{
    QString selectedFilter, ext;
    if(selectedExtension)
        return browseSaveFile(caption, browsePath, filter, selectedFilter, *selectedExtension);
    else
        return browseSaveFile(caption, browsePath, filter, selectedFilter, ext);
}

QString MessageForwarder::browseOpenFolder(QString caption, QString browsePath)
{
	if(useNativeFileDialogs())	return QFileDialog::getExistingDirectory(nullptr, caption, browsePath, QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);
	else						return QFileDialog::getExistingDirectory(nullptr, caption, browsePath, QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks | QFileDialog::DontUseNativeDialog);
}

QString MessageForwarder::browseOpenFolder(QString caption)
{
	return browseOpenFolder(caption, constrainToSandboxStartDir(AppDirs::documents()));
}
