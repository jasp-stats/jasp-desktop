
#include "mainwindow.h"
#include <QApplication>
#include <QDialog>
#include <QGridLayout>
#include <QLayout>
#include <QDebug>

int main(int argc, char *argv[])
{
	//Q_INIT_RESOURCE(html);

    QApplication a(argc, argv);
	MainWindow w;

    w.show();

	if (argc > 1)
		w.open(QString(argv[1]));
    
    return a.exec();
}
