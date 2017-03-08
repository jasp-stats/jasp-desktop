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

#ifndef R11TLEARNFORM_H
#define R11TLEARNFORM_H

#include "../analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"

namespace Ui {
class R11tLearnForm;
}

class R11tLearnForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit R11tLearnForm(QWidget *parent = 0);
	~R11tLearnForm();

private:
	Ui::R11tLearnForm *ui;

	TableModelVariablesAssigned *_subjectIdListModel;
	TableModelVariablesAssigned *_groupListModel;
	TableModelVariablesAssigned *_trialNumberListModel;
	TableModelVariablesAssigned *_deckListModel;
	TableModelVariablesAssigned *_rewardListModel;
	TableModelVariablesAssigned *_lossListModel;

};

#endif // R11TLEARNFORM_H
