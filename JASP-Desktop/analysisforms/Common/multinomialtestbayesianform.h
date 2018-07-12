//
// Copyright (C) 2018 University of Amsterdam
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

#ifndef MULTINOMIALTESTBAYESIANFORM_H
#define MULTINOMIALTESTBAYESIANFORM_H

#include <QTableWidgetItem>
#include <QStringList>

#include "../analysisform.h"
#include "options/optionvariable.h"

#include "columnsmodel.h"

namespace Ui {
class MultinomialTestBayesianForm;
}

class MultinomialTestBayesianForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit MultinomialTestBayesianForm(QWidget *parent = 0);
	~MultinomialTestBayesianForm();
	void addColumnToTable();
	bool deleteColumnFromTable();
	void resetTable();
	void setTableVerticalHeaders();

	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void on_addColumn_clicked(bool checked);
	void on_deleteColumn_clicked(bool checked);
	void on_resetColumns_clicked(bool checked);
	void addFixedFactors();
	void expectedCountsHandler();
	void countModelHandler();
	void cellChangedHandler();
	void loadQML();

private:
	Ui::MultinomialTestBayesianForm *ui;
	QStringList verticalLabels;
	QStringList horizontalLabels;
	TableModelVariablesAssigned *factorModel;
	std::string _previous;
	Options *_options;

	ColumnsModel *_columnsModel = NULL;
};

#endif // MULTINOMIALTESTBAYESIANFORM_H
