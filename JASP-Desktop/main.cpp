
#include "mainwindow.h"
#include <QApplication>
#include <QDialog>
#include <QGridLayout>
#include <QLayout>
#include <QDebug>

#include "application.h"

int main(int argc, char *argv[])
{
	QCoreApplication::setOrganizationName("JASP");
	QCoreApplication::setOrganizationDomain("jasp-stats.org");
	QCoreApplication::setApplicationName("JASP");

	Application a(argc, argv);
	MainWindow w;

    w.show();

	if (argc > 1)
		w.open(QString(argv[1]));
    
    return a.exec();
}
