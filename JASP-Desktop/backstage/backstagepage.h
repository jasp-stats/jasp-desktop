#ifndef BACKSTAGEPAGE_H
#define BACKSTAGEPAGE_H

#include <QWidget>

#include "fileevent.h"

class BackstagePage : public QWidget
{
	Q_OBJECT
public:
	explicit BackstagePage(QWidget *parent = 0);

	virtual void setMode(FileEvent::FileMode mode);

signals:
	void dataSetIORequest(FileEvent *event);

	void closeDataSetSelected();
	void exportSelected(QString filename);

protected:
	FileEvent::FileMode _mode;

};

#endif // BACKSTAGEPAGE_H
