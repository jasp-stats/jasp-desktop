#include "mainwindow.h"
#include <QApplication>
#include <QDialog>
#include <QGridLayout>
#include <QLayout>

int main(int argc, char *argv[])
{
	//Q_INIT_RESOURCE(html);

    QApplication a(argc, argv);
    MainWindow w;

    w.show();
    
    return a.exec();
}
