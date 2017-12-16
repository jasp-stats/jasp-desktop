#ifndef BACKSTAGEEXAMPLES_H
#define BACKSTAGEEXAMPLES_H

#include "backstagepage.h"
#include "breadcrumbs.h"
#include "fsbrowser.h"
#include "fsbmexamples.h"

#include <QPushButton>
#include <QLineEdit>
#include <QDir>

class BackstageExamples : public BackstagePage
{
	
	Q_OBJECT
public:
	explicit BackstageExamples(QWidget *parent = 0);
	
private slots:
	void notifyDataSetOpened(QString path);
	
private:
	BreadCrumbs *_breadCrumbs;
	FSBMExamples *_model;
	FSBrowser *_fsBrowser;
	QWidget *_fileNameContainer;
	QString _currentFileName;
	
};

#endif // BACKSTAGEEXAMPLES_H
