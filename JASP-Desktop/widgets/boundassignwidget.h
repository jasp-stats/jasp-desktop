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

#ifndef BOUNDASSIGNWIDGET_H
#define BOUNDASSIGNWIDGET_H

#include <QWidget>

#include "bound.h"
#include "common.h"
#include "options/optionterms.h"
#include "widgets/tablemodelvariablesavailable.h"
#include "widgets/tablemodelvariablesassigned.h"

#include <QAbstractItemModel>

namespace Ui {
class BoundAssignWidget;
}

class BoundAssignWidget : public QWidget, public Bound
{
	Q_OBJECT

public:
	explicit BoundAssignWidget(QWidget *parent = 0);
	~BoundAssignWidget();

	virtual void bindTo(Option *option) OVERRIDE;

	void setVariables(const Terms &variables);
	void setLabels(const QString &left, const QString &right);

private:
	Ui::BoundAssignWidget *ui;

	TableModelVariablesAvailable *_availableModel;
	TableModelVariablesAssigned *_assignedModel;
};

#endif // BOUNDASSIGNWIDGET_H
