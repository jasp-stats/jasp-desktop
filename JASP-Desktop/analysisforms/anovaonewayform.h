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

#ifndef ONEWAYANOVAFORM_H
#define ONEWAYANOVAFORM_H

#include "options/options.h"
#include "dataset.h"

#include "analysisform.h"

#include "availablefields.h"
#include "widgets/tablemodelcontrasts.h"

namespace Ui {
class AnovaOneWayForm;
}

class AnovaOneWayForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaOneWayForm(QWidget *parent = 0);
	~AnovaOneWayForm();

private slots:
	void groupingVariableChanged();
	void contrastsClicked(QModelIndex index);

private:
	Ui::AnovaOneWayForm *ui;

	TableModelVariablesAssigned _variablesModel;
	TableModelVariablesAssigned _groupingVariableModel;
	TableModelContrasts _contrastsModel;

};

#endif // ONEWAYANOVAFORM_H
