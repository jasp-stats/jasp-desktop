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


#ifndef REINFORCEMENTLEARNINGR11TLEARNINGFORM_H
#define REINFORCEMENTLEARNINGR11TLEARNINGFORM_H

#include "analysis/analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"

namespace Ui {
class ReinforcementLearningR11tLearningForm;
}

class ReinforcementLearningR11tLearningForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit ReinforcementLearningR11tLearningForm(QWidget *parent = 0);
	~ReinforcementLearningR11tLearningForm();

private:
	Ui::ReinforcementLearningR11tLearningForm *ui;

	TableModelVariablesAssigned *_subjectIdListModel;
	TableModelVariablesAssigned *_groupListModel;
	TableModelVariablesAssigned *_trialNumberListModel;
	TableModelVariablesAssigned *_deckListModel;
	TableModelVariablesAssigned *_rewardListModel;
	TableModelVariablesAssigned *_lossListModel;
};

#endif // REINFORCEMENTLEARNINGR11TLEARNINGFORM_H
