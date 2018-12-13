#include "messageforwarder.h"
#include "mainwindow.h"

MessageForwarder::MessageForwarder(MainWindow *main) : QObject(main)
{
	if(singleton != nullptr)
		throw std::runtime_error("There can be only ONE MessageForwarder!");

	singleton = this;

	connect(this, &MessageForwarder::showWarningSignal, main, &MainWindow::showWarning);
}

MessageForwarder * MessageForwarder::singleton = nullptr;

void MessageForwarder::showWarning(QString title, QString message)
{
	emit singleton->showWarningSignal(title, message);
}
