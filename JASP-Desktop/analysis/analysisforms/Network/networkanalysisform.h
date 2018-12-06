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

#ifndef NETWORKANALYSISFORM_H
#define NETWORKANALYSISFORM_H

#include "analysis/analysisform.h"

namespace Ui {
class NetworkAnalysisForm;
}

class NetworkAnalysisForm : public AnalysisForm
{
    Q_OBJECT

public:
    explicit NetworkAnalysisForm(QWidget *parent = 0);
    ~NetworkAnalysisForm();

private slots:
    void on_estimator_currentIndexChanged(const QString &);
    void on__4cv_clicked();
    void on__3stars_clicked();
    void on__2ric_clicked();
    void on__1ebic_clicked();
    void on_graphicalOptionsExpander_clicked();
    void on__1spring_clicked(bool checked);
    void on__2circle_clicked(bool checked);
    void on__3Data_clicked(bool checked);

private:
    Ui::NetworkAnalysisForm *ui;
};

#endif // NETWORKANALYSISFORM_H
