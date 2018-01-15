//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef BAINTEXTEDIT_H
#define BAINTEXTEDIT_H

#include <QTextEdit>
#include <QLabel>

#include "bound.h"
#include "common.h"
#include "textmodelbain.h"

class BainTextEdit : public QTextEdit, public Bound
{
	Q_OBJECT
public:
	explicit BainTextEdit(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	void populateFromOption(Option *option);
	
signals:
	void applyRequest();

private slots:
	void contentsChangedHandler();

protected:
	void keyPressEvent(QKeyEvent *event) OVERRIDE;
	void insertFromMimeData(const QMimeData *source) OVERRIDE;

private:
	TextModelBain *_model;

};

#endif // BAINTEXTEDIT_H
