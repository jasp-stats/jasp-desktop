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

#ifndef ELIDELABEL_H
#define ELIDELABEL_H

#include <QLabel>

#include "common.h"

class ElideLabel : public QLabel
{
	Q_OBJECT
public:
	explicit ElideLabel(QWidget *parent = 0);

	QSize minimumSizeHint() const OVERRIDE;
	QSize sizeHint() const OVERRIDE;

protected:
	void resizeEvent(QResizeEvent *event) OVERRIDE;

private:
	QString _originalText;
	QString _modifiedText;
};

#endif // ELIDELABEL_H
