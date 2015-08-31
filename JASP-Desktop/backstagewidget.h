#ifndef BACKSTAGEWIDGET_H
#define BACKSTAGEWIDGET_H

#include <QWidget>

#include <QStackedWidget>

#include "backstage/verticaltabbar.h"
#include "backstage/opensavewidget.h"
#include "fileevent.h"

#include "activitylog.h"

class BackStageWidget : public QWidget
{
	Q_OBJECT
public:
	explicit BackStageWidget(QWidget *parent = NULL);

	void setLog(ActivityLog *log);
	FileEvent *open();
	FileEvent *open(const QString &filepath);
	FileEvent *save();
	FileEvent *close();

signals:
	void dataSetIORequest(FileEvent *event);
	void exportSelected(QString filename);

private slots:
	void tabPageChanging(int index, bool &cancel);

	void dataSetIORequestHandler(FileEvent *event);
	void dataSetIORequestCompleted(FileEvent *event);

private:
	VerticalTabBar *_tabBar;
	QStackedWidget *_tabPages;

	OpenSaveWidget *_openAndSaveWidget;

	bool _dataSetHasPathAndIsntReadOnly;
};

#endif // BACKSTAGEWIDGET_H
