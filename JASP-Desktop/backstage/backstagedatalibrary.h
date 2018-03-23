#ifndef BACKSTAGEDATALIBRARY_H
#define BACKSTAGEDATALIBRARY_H

#include "backstagepage.h"
#include "fsbrowser.h"
#include "fsbmexamples.h"
#include "datalibrarylistmodel.h"
#include "datalibrarybreadcrumbsmodel.h"

#include <QPushButton>
#include <QLineEdit>
#include <QDir>
#include <QQmlContext>

namespace Ui {
class BackstageDataLibrary;
}

class BackstageDataLibrary : public BackstagePage
{
	
	Q_OBJECT
public:
	explicit BackstageDataLibrary(QWidget *parent = 0);
	~BackstageDataLibrary();

public slots:
	void openFile(FileEvent *event);
	
private:	
	DataLibraryListModel *_dataLibraryListModel;
	DataLibraryBreadCrumbsModel *_dataLibraryBreadCrumbsModel;	
	Ui::BackstageDataLibrary *ui;
			
};

#endif // BACKSTAGEDATALIBRARY_H
