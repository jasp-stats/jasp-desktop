#include "application.h"

#include <iostream>

Application::Application(int &argc, char **argv) :
	QApplication(argc, argv)
{
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
		//return false;
	}
}
