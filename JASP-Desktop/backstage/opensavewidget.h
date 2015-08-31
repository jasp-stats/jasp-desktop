#ifndef OPENWIDGET_H
#define OPENWIDGET_H

#include <QWidget>

#include <QWebView>

#include "verticaltabwidget.h"
#include "fsbmrecent.h"
#include "fsbmexamples.h"
#include "fsbrowser.h"

#include "backstagecomputer.h"
#include "backstageosf.h"

#include "fileevent.h"

class OpenSaveWidget : public QWidget
{
	Q_OBJECT
public:
	explicit OpenSaveWidget(QWidget *parent = 0);

	VerticalTabWidget *tabWidget();
	void setSaveMode(FileEvent::FileMode mode);

	FileEvent* open();
	FileEvent* open(const QString &path);
	FileEvent* save();
	FileEvent *close();

public slots:
	void dataSetIOCompleted(FileEvent *event);

signals:
	void dataSetIORequest(FileEvent *event);

private slots:
	void dataSetIORequestHandler(FileEvent *event);
	void dataSetOpenRequestHandler(QString path);
	void dataSetOpenExampleRequestHandler(QString path);

private:
	bool _currentFileHasPath;
	QString _currentFilePath;
	bool _currentFileReadOnly;

	FileEvent::FileMode _mode;

	VerticalTabWidget *_tabWidget;

	FSBMRecent   *_fsmRecent;
	FSBMExamples *_fsmExamples;

	FSBrowser *_bsRecent;
	BackstageComputer *_bsComputer;
	BackstageOSF *_bsOSF;
	FSBrowser *_bsExamples;

};

#endif // OPENWIDGET_H
