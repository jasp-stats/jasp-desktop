
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

	QLocale::setDefault(QLocale(QLocale::English)); // make decimal points == .

	Application a(argc, argv);
    return a.exec();
}
