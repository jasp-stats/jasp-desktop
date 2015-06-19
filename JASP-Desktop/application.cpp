
#include "application.h"

#include <QFileOpenEvent>

#include <iostream>

Application::Application(int &argc, char **argv) :
	QApplication(argc, argv)
{
	_mainWindow = new MainWindow();
	_mainWindow->show();

	QStringList args = QApplication::arguments();

	if (args.length() > 1)
		_mainWindow->open(args.at(1));
}

bool Application::notify(QObject *receiver, QEvent *event)
{
	try
	{
		return QApplication::notify(receiver, event);
	}
	catch (std::exception &e)
	{
		std::cout << e.what() << "\n";
		std::cout.flush();

		throw e;
	}
}

bool Application::event(QEvent *event)
{
	if (event->type() == QEvent::FileOpen)
	{
		QFileOpenEvent *openEvent = static_cast<QFileOpenEvent*>(event);
		QString file = openEvent->file();
		_mainWindow->open(file);

		return true;
	}
	else
	{
		return QApplication::event(event);
	}
}
