
#include "backstagewidget.h"

#include <QHBoxLayout>
#include <QLabel>
#include <QFileInfo>

BackStageWidget::BackStageWidget(QWidget *parent) : QWidget(parent)
{
	_dataSetHasPathAndIsntReadOnly = false;

	_tabBar = new VerticalTabBar(this);
	_tabPages = new QStackedWidget(this);

	_openAndSaveWidget = new OpenSaveWidget();

	QHBoxLayout *layout = new QHBoxLayout(this);
	layout->setSpacing(0);
	layout->setContentsMargins(0, 0, 0, 0);

	layout->addWidget(_tabBar);
	layout->addWidget(_tabPages);

	QString styleSheet;

	QFile firstStyleSheetFile(":/backstage/firsttabsstylesheet.qss");
	firstStyleSheetFile.open(QFile::ReadOnly);
	styleSheet = QString::fromUtf8(firstStyleSheetFile.readAll());

	_tabBar->setMinimumWidth(150);
	_tabBar->setStyleSheet(styleSheet);


	QFile secondStyleSheetFile(":/backstage/secondtabsstylesheet.qss");
	secondStyleSheetFile.open(QFile::ReadOnly);
	styleSheet = QString::fromUtf8(secondStyleSheetFile.readAll());

	_openAndSaveWidget->tabWidget()->tabBar()->setMinimumWidth(200);
	_openAndSaveWidget->tabWidget()->tabBar()->setStyleSheet(styleSheet);

	_tabPages->addWidget(_openAndSaveWidget);

	_tabBar->addTab("Open");
	_tabBar->addTab("Save");
	_tabBar->addTab("Save As");
	_tabBar->addTab("Export");
	_tabBar->addTab("Close");

	_tabBar->setTabEnabled(1, false);
	_tabBar->setTabEnabled(2, false);
	_tabBar->setTabEnabled(3, false);
	_tabBar->setTabEnabled(4, false);

	connect(_openAndSaveWidget, SIGNAL(dataSetIORequest(FileEvent*)), this, SLOT(dataSetIORequestHandler(FileEvent*)));
	connect(_tabBar, SIGNAL(currentChanging(int,bool&)), this, SLOT(tabPageChanging(int,bool&)));
}

void BackStageWidget::setLog(ActivityLog *log)
{

}

FileEvent *BackStageWidget::open()
{
	return _openAndSaveWidget->open();
}

FileEvent *BackStageWidget::open(const QString &filepath)
{
	return _openAndSaveWidget->open(filepath);
}

FileEvent *BackStageWidget::save()
{
	return _openAndSaveWidget->save();
}

FileEvent *BackStageWidget::close()
{
	return _openAndSaveWidget->close();
}

void BackStageWidget::dataSetIORequestHandler(FileEvent *event)
{
	connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIORequestCompleted(FileEvent*)));
	emit dataSetIORequest(event);
}

void BackStageWidget::dataSetIORequestCompleted(FileEvent *event)
{
	if (event->successful())
	{
		if (event->operation() == FileEvent::FileOpen)
		{
			_dataSetHasPathAndIsntReadOnly = ! event->isReadOnly();

			_tabBar->setTabEnabled(1, true);
			_tabBar->setTabEnabled(2, true);
			_tabBar->setTabEnabled(3, true);
			_tabBar->setTabEnabled(4, true);
		}
		else if (event->operation() == FileEvent::FileSave)
		{
			_dataSetHasPathAndIsntReadOnly = true;
		}
		else if (event->operation() == FileEvent::FileClose)
		{
			_dataSetHasPathAndIsntReadOnly = true;
			_tabBar->setTabEnabled(1, false);
			_tabBar->setTabEnabled(2, false);
			_tabBar->setTabEnabled(3, false);
			_tabBar->setTabEnabled(4, false);
		}
	}
}

void BackStageWidget::tabPageChanging(int index, bool &cancel)
{
	switch (index)
	{
	case 0:  // Open
		_openAndSaveWidget->setSaveMode(FileEvent::FileOpen);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;
	case 1:  // Save
		_openAndSaveWidget->setSaveMode(FileEvent::FileSave);
		if (_dataSetHasPathAndIsntReadOnly)
		{
			_openAndSaveWidget->save();
		}
		else
		{
			_tabBar->setCurrentIndex(2);
			_tabPages->setCurrentWidget(_openAndSaveWidget);
		}
		cancel = true;
		break;
	case 2:  // Save As
		_openAndSaveWidget->setSaveMode(FileEvent::FileSave);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;
	case 3:  // Export
		break;
	case 4: // Close
		_openAndSaveWidget->close();
		_tabBar->setCurrentIndex(0);
		_openAndSaveWidget->setSaveMode(FileEvent::FileOpen);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		cancel = true;
		break;
	}
}

