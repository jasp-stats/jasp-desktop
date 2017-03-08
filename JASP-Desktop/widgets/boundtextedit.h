//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef BOUNDTEXTEDIT_H
#define BOUNDTEXTEDIT_H

#include <QTextEdit>
#include <QLabel>

#include "bound.h"
#include "common.h"
#include "textmodellavaan.h"

class BoundTextEdit : public QTextEdit, public Bound
{
	Q_OBJECT
public:
	explicit BoundTextEdit(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;

signals:

public slots:

private slots:
	void cursorPositionChangedHandler();
	void errorStateChangedHandler();
	void contentsChangedHandler();

protected:
	void keyPressEvent(QKeyEvent *event) OVERRIDE;
	void resizeEvent(QResizeEvent *e) OVERRIDE;
	void paintEvent(QPaintEvent *event) OVERRIDE;
	void insertFromMimeData(const QMimeData *source) OVERRIDE;

private:

	QString _errorStylesheet;
	QString _okStylesheet;
	QString _okMessage;

	bool _applied;
	QLabel *_status;
	TextModelLavaan *_model;

};

#endif // BOUNDTEXTEDIT_H
