#include "messageforwarder.h"
#include "mainwindow.h"
#include <QMessageBox>
#include <QFileDialog>

MessageForwarder::MessageForwarder(MainWindow *main) : QObject(main), _main(main)
{
	if(singleton != nullptr)
		throw std::runtime_error("There can be only ONE MessageForwarder!");

	singleton = this;

	connect(this, &MessageForwarder::showWarningSignal, main, &MainWindow::showWarning);
}

MessageForwarder * MessageForwarder::singleton = nullptr;

void MessageForwarder::showWarning(QString title, QString message)
{
	//emit singleton->showWarningSignal(title, message);
	QMessageBox::warning(nullptr, title, message);
}

bool MessageForwarder::showYesNo(QString title, QString message, QString YesButtonText, QString NoButtonText)
{
	QMessageBox box(QMessageBox::Question, title, message,  QMessageBox::Yes | QMessageBox::No);

	box.setButtonText(QMessageBox::Yes,		YesButtonText);
	box.setButtonText(QMessageBox::No,		NoButtonText);

	return box.exec() == QMessageBox::Yes;
}

MessageForwarder::DialogResponse MessageForwarder::showYesNoCancel(QString title, QString message, QString YesButtonText, QString NoButtonText, QString CancelButtonText)
{
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

MessageForwarder::DialogResponse MessageForwarder::showSaveDiscardCancel(QString title, QString message)
{
	switch(QMessageBox::question(nullptr, title, message, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel))
	{
	case QMessageBox::Save:			return DialogResponse::Save;
	case QMessageBox::Discard:		return DialogResponse::Discard;
	default:						return DialogResponse::Cancel;
	}
}

QString MessageForwarder::browseOpenFile(QString caption, QString browsePath, QString filter)
{
	return 	QFileDialog::getOpenFileName(nullptr, caption, browsePath, filter);
}

QString MessageForwarder::browseSaveFile(QString caption, QString browsePath, QString filter, QString * selectedFilter)
{
	return 	QFileDialog::getSaveFileName(nullptr, caption, browsePath, filter, selectedFilter);
}

QString MessageForwarder::browseOpenFolder(QString caption, QString browsePath)
{
	return QFileDialog::getExistingDirectory(nullptr, caption, browsePath, QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);
}
