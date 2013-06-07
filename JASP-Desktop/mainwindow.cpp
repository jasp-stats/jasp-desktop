#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <iostream>
#include <fstream>

#include <QString>
#include <QFileDialog>
#include <QGridLayout>
#include <QLayout>
#include <QDebug>
#include <QWebFrame>
#include <QWebElement>

#include "backstageform.h"


using namespace std;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::exitSelectedHandler()
{
	QApplication::exit(0);
}
