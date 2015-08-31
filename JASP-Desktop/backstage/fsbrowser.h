#ifndef FSBROWSER_H
#define FSBROWSER_H

#include <QWidget>
#include <QGridLayout>
#include <QButtonGroup>

#include "fsbmodel.h"
#include "breadcrumbs.h"

class FSBrowser : public QWidget
{
	Q_OBJECT
public:
	explicit FSBrowser(QWidget *parent = 0);

	void setFSModel(FSBModel *model);

	enum BrowseMode { BrowseOpenFile, BrowseOpenFolder, BrowseSaveFile };
	enum ViewType   { IconView, ListView };

	void setBrowseMode(BrowseMode mode);
	void setViewType(ViewType viewType);

signals:

	void entryOpened(QString path);

public slots:

private slots:

	void refresh();
	void entryOpenedHandler();

private:

	BrowseMode _browseMode;
	ViewType _viewType;

	QWidget *_scrollPane;
	QVBoxLayout *_scrollPaneLayout;
	QButtonGroup *_buttonGroup;

	FSBModel *_model;


};

#endif // FSBROWSER_H
