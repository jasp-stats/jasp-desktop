#ifndef BACKSTAGECOMPUTER_H
#define BACKSTAGECOMPUTER_H

#include "backstagepage.h"

#include "fsbmrecentfolders.h"

namespace Ui {
class BackstageComputer;
}

class BackstageComputer : public BackstagePage
{
	Q_OBJECT

public:
	explicit BackstageComputer(QWidget *parent = 0);
	~BackstageComputer();

	FileEvent *browseOpen(const QString &path = "");
	FileEvent *browseSave(const QString &path = "");

	void addRecent(const QString &path);

	void setFileName(const QString &filename);
	void clearFileName();

signals:
	void dataSetIORequest(FileEvent *event);

protected:
	bool eventFilter(QObject *object, QEvent *event) OVERRIDE;

private slots:
	void selectionMade(QString path);
	void browseSelected();

private:
	// these two variables are a hack!
	bool _hasFileName;
	QString _fileName;

	Ui::BackstageComputer *ui;

	FSBMRecentFolders *_model;
};

#endif // BACKSTAGECOMPUTER_H
