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

#ifndef RELIABILITYANALYSISFORM_H
#define RELIABILITYANALYSISFORM_H

#include "analysisform.h"

namespace Ui {
class ReliabilityAnalysisForm;
}

class ReliabilityAnalysisForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit ReliabilityAnalysisForm(QWidget *parent = 0);
	~ReliabilityAnalysisForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void variablesChanging();
	void variablesChanged();

private:
	Ui::ReliabilityAnalysisForm *ui;

	TableModelVariablesAssigned *_variableListModel;
};

#endif // RELIABILITYANALYSISFORM_H
