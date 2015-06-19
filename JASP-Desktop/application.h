#ifndef APPLICATION_H
#define APPLICATION_H

#include <QApplication>
#include "mainwindow.h"

#include "common.h"

class Application : public QApplication
{
	Q_OBJECT
public:
	explicit Application(int &argc, char **argv);

	virtual bool notify(QObject *receiver, QEvent *event) OVERRIDE;
	virtual bool event(QEvent *event) OVERRIDE;

signals:

public slots:

private:
	MainWindow *_mainWindow;

};

#endif // APPLICATION_H
