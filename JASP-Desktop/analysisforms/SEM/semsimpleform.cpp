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

#include "semsimpleform.h"
#include "ui_semsimpleform.h"
#include "widgets/itemmodelselectvariable.h"

SEMSimpleForm::SEMSimpleForm(QWidget *parent) :
	AnalysisForm("SEMSimpleForm", parent),
	ui(new Ui::SEMSimpleForm)
{
	ui->setupUi(this);
	
	ItemModelSelectVariable *model = new ItemModelSelectVariable(this);
	model->setSource(&_availableVariablesModel);

	ui->groupingVariable->setModel(model);

	ui->containerStatistics->hide();
	ui->containerOptions->hide();
	ui->containerAdvanced->hide();
	
	connect(ui->model, &BoundTextEdit::applyRequest, this, &SEMSimpleForm::checkSyntax);
}

SEMSimpleForm::~SEMSimpleForm()
{
	delete ui;
}

void SEMSimpleForm::checkSyntax()
{
	// create an R vector of available column names
	// TODO: Proper handling of end-of-string characters and funny colnames
	QString colNames = "c(";
	bool firstCol = true;
	QList<QString> vars = _availableVariablesModel.allVariables().asQList();
	for (QString &var : vars)
	{
		if (!firstCol)
			colNames.append(',');
		colNames.append('\'')
				.append(var.replace("\'", "\\u0027")
						   .replace("\"", "\\u0022")
						   .replace("\\", "\\\\"))
				.append('\'');
		firstCol = false;
	}
	colNames.append(')');
	
	// Get lavaan model code
	QString lavaanCode = ui->model->toPlainText();
	// replace ' and " with their unicode counterparts
	// This protects against arbitrary code being run through string escaping.
	lavaanCode.replace("\'", "\\u0027").replace("\"", "\\u0022");
	// This protects against crashes due to backslashes
	lavaanCode.replace("\\", "\\\\");
	
	// Create R code string	
	QString checkCode = "checkLavaanModel('";
	checkCode
		.append(lavaanCode)
		.append("', ")
		.append(colNames)
		.append(")");
	
	runRScript(checkCode);
}

void SEMSimpleForm::rScriptDoneHandler(QVariant key, const QString & result)
{
	ui->model->applyModel(result);
}
