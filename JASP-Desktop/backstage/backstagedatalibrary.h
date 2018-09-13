#ifndef BACKSTAGEDATALIBRARY_H
#define BACKSTAGEDATALIBRARY_H

#include "backstagepage.h"
#include "fsbmdatalibrary.h"
#include "datalibrarylistmodel.h"
#include "datalibrarybreadcrumbsmodel.h"

#include <QPushButton>
#include <QLineEdit>
#include <QDir>
#include <QQmlContext>

namespace Ui {
class BackstageForm;
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
	DataLibraryBreadCrumbsListModel *_dataLibraryBreadCrumbsListModel;	
	Ui::BackstageForm *ui;
			
};

#endif // BACKSTAGEDATALIBRARY_H
