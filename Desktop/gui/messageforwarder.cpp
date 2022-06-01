#include "messageforwarder.h"
#include "mainwindow.h"
#include <QMessageBox>
#include <QFileDialog>
#include <QRegularExpression>
#include "utilities/settings.h"
#include "utilities/appdirs.h"

MessageForwarder::MessageForwarder(MainWindow *main) : QQuickItem(nullptr), _main(main)
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

MessageForwarder * MessageForwarder::_singleton = nullptr;

void MessageForwarder::showWarning(QString title, QString message)
{
	QMessageBox::warning(nullptr, title, message);
}

bool MessageForwarder::showYesNo(QString title, QString message, QString YesButtonText, QString NoButtonText)
{
	if(YesButtonText == "")		YesButtonText	= tr("Yes");
	if(NoButtonText == "")		NoButtonText	= tr("No");

	QMessageBox box(QMessageBox::Question, title, message,  QMessageBox::Yes | QMessageBox::No);

	box.setButtonText(QMessageBox::Yes,		YesButtonText);
	box.setButtonText(QMessageBox::No,		NoButtonText);

	return box.exec() == QMessageBox::Yes;
}

MessageForwarder::DialogResponse MessageForwarder::showYesNoCancel(QString title, QString message, QString YesButtonText, QString NoButtonText, QString CancelButtonText)
{
	if(YesButtonText == "")		YesButtonText		= tr("Yes");
	if(NoButtonText == "")		NoButtonText		= tr("No");
	if(CancelButtonText == "")	CancelButtonText	= tr("Cancel");

	QMessageBox box(QMessageBox::Question, title, message,  QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel);

	box.setButtonText(QMessageBox::Yes,		YesButtonText);
	box.setButtonText(QMessageBox::No,		NoButtonText);
	box.setButtonText(QMessageBox::Cancel,	CancelButtonText);

	switch(box.exec())
	{
	case QMessageBox::Yes:	return DialogResponse::Yes;
	case QMessageBox::No:	return DialogResponse::No;
	default:				return DialogResponse::Cancel;
	}
}

MessageForwarder::DialogResponse MessageForwarder::showSaveDiscardCancel(QString title, QString message, QString saveTxt, QString discardText, QString cancelText)
{
	QMessageBox box(QMessageBox::Question, title, message,  QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);

	if(saveTxt == "")		saveTxt		= tr("Save");
	if(discardText == "")	discardText = tr("Don't Save");
	if(cancelText == "")	cancelText	= tr("Cancel");

	box.setButtonText(QMessageBox::Save,		saveTxt);
	box.setButtonText(QMessageBox::Discard,		discardText);
	box.setButtonText(QMessageBox::Cancel,		cancelText);

	switch(box.exec())
	{
	case QMessageBox::Save:			return DialogResponse::Save;
	case QMessageBox::Discard:		return DialogResponse::Discard;
	default:						return DialogResponse::Cancel;
	}
}

QString MessageForwarder::browseOpenFile(QString caption, QString browsePath, QString filter)
{
	if(Settings::value(Settings::USE_NATIVE_FILE_DIALOG).toBool())	return 	QFileDialog::getOpenFileName(nullptr, caption, browsePath, filter);
	else															return 	QFileDialog::getOpenFileName(nullptr, caption, browsePath, filter, nullptr, QFileDialog::DontUseNativeDialog);
}

QString MessageForwarder::browseOpenFileDocuments(QString caption, QString filter)
{
	return browseOpenFile(caption, AppDirs::documents(), filter);
}

QString MessageForwarder::browseSaveFileDocuments(QString caption, QString filter)
{
	return browseSaveFile(caption, AppDirs::documents(), filter);
}

QString MessageForwarder::browseSaveFile(QString caption, QString browsePath, QString filter, QString * selectedExtension)
{
	Log::log() << "MessageForwarder::browseSaveFile(\"" << caption.toStdString() << "\", \"" << browsePath.toStdString() << "\", \"" << filter.toStdString() << "\")" << std::endl;

	QString saveFileName, selectedFilter;

	if(Settings::value(Settings::USE_NATIVE_FILE_DIALOG).toBool())	saveFileName = 	QFileDialog::getSaveFileName(nullptr, caption, browsePath, filter, &selectedFilter);
	else															saveFileName = 	QFileDialog::getSaveFileName(nullptr, caption, browsePath, filter, &selectedFilter, QFileDialog::DontUseNativeDialog);

	Log::log() << "Selected save file: " << saveFileName << " and selected filter: " << selectedFilter << std::endl;

	//Lets make sure the extension is added:
	const QRegularExpression extReg("\\*\\.(\\w+)");
	QRegularExpressionMatch  possibleMatch = extReg.match(selectedFilter);

	if(possibleMatch.hasMatch())
	{
		QString ext = possibleMatch.captured(1);

		if(!saveFileName.endsWith(ext))
			saveFileName += "." + ext;

		if(selectedExtension)
			*selectedExtension = ext;
	}
	else if(selectedExtension)//So the filter doesnt tell us the extension but the caller expects to know what is what
	{
		if(saveFileName.lastIndexOf('.') >= 0)	*selectedExtension = saveFileName.mid(saveFileName.lastIndexOf('.') + 1);
		else									*selectedExtension = ""; //???
	}

	if(selectedExtension)
		Log::log() << "Selected extension: '" << *selectedExtension << "'" << std::endl;

	return saveFileName;
}

QString MessageForwarder::browseOpenFolder(QString caption, QString browsePath)
{
	if(Settings::value(Settings::USE_NATIVE_FILE_DIALOG).toBool())	return QFileDialog::getExistingDirectory(nullptr, caption, browsePath, QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);
	else															return QFileDialog::getExistingDirectory(nullptr, caption, browsePath, QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks | QFileDialog::DontUseNativeDialog);
}
