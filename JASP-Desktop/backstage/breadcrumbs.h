#ifndef BREADCRUMBS_H
#define BREADCRUMBS_H

#include <QWidget>

#include <QHBoxLayout>
#include <QToolButton>
#include <QButtonGroup>
#include <QMenu>

#include "fsbmodel.h"

#include "common.h"

class BreadCrumbs : public QWidget
{
	Q_OBJECT
public:
	explicit BreadCrumbs(QWidget *parent = 0);

	void setRootPath(const QString &path);
	const QString &path() const;

	void setModel(FSBModel *model);

private slots:
	void setPath(QString path);
	void buttonClicked();
	void dotDotDotClicked();

protected:
	void resizeEvent(QResizeEvent *event) OVERRIDE;

private:

	void populate();
	void refresh(const QSize &size);

	FSBModel *_model;

	int _dotDotIndex;

	QHBoxLayout *_layout;
	QToolButton *_dotDotDotButton;
	QButtonGroup *_buttons;

	QString _rootPath;
	QString _path;

	QStringList _rootPieces;
	QStringList _pathPieces;
};

#endif // BREADCRUMBS_H
