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

#ifndef CONTINGENCYTABLESBAYESIANFORM_H
#define CONTINGENCYTABLESBAYESIANFORM_H

#include <QWidget>
#include "analysisform.h"
#include "widgets/tablemodelvariableslevels.h"

namespace Ui {
class ContingencyTablesBayesianForm;
}

class ContingencyTablesBayesianForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit ContingencyTablesBayesianForm(QWidget *parent = 0);
	~ContingencyTablesBayesianForm();

private slots:
	void independentMultinomialSamplingToggled(bool on);
	void otherSamplingToggled(bool on);

private:
	Ui::ContingencyTablesBayesianForm *ui;

	TableModelVariablesAssigned *_rowsModel;
	TableModelVariablesAssigned *_columnsModel;
	TableModelVariablesAssigned *_countsModel;
	TableModelVariablesLevels *_layersModel;
};

#endif // CONTINGENCYTABLESBAYESIANFORM_H
