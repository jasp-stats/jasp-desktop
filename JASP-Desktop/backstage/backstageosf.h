#ifndef BACKSTAGEOSF_H
#define BACKSTAGEOSF_H

#include "backstagepage.h"

#include <QPushButton>

#include "fsbmcomputer.h"
#include "fsbrowser.h"
#include "breadcrumbs.h"

class BackstageOSF : public BackstagePage
{
	Q_OBJECT
public:
	explicit BackstageOSF(QWidget *parent = 0);

signals:
	void dataSetOpened(QString path);

private slots:
	void notifyDataSetOpened(QString path);

private:
	BreadCrumbs *_breadCrumbs;
	FSBMComputer *_model;
	FSBrowser *_fsBrowser;
	QPushButton *_browseButton;
};

#endif // BACKSTAGEOSF_H
