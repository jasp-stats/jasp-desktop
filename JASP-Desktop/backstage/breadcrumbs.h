//
// Copyright (C) 2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

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
