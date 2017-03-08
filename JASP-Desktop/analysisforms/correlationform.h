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

#ifndef CORRELATIONFORM_H
#define CORRELATIONFORM_H

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"

namespace Ui {
class CorrelationForm;
}

class CorrelationForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit CorrelationForm(QWidget *parent = 0);
	~CorrelationForm();

private:
	Ui::CorrelationForm *ui;

	TableModelVariablesAssigned *_modelVariables;
};

#endif // CORRELATIONFORM_H
