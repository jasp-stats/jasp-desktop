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

#include "subjectivepriorswidget.h"
#include "ui_subjectivepriorswidget.h"


SubjectivePriorsWidget::SubjectivePriorsWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::SubjectivePriorsWidget)
{
	ui->setupUi(this);

	// default
	ui->effectSizeStandardized->setEnabled(true);
	ui->dienesEffectSize->setEnabled(false);

	ui->defaultStandardizedEffectSize->setEnabled(true);
	ui->informativeStandardizedEffectSize->setEnabled(false);
	ui->cauchyInformative->setEnabled(true);

	ui->label_informativeNormalMean->hide();
	ui->label_informativeNormalStd->hide();
	ui->label_informativeTLocation->hide();
	ui->label_informativeTScale->hide();
	ui->label_informativeTDf->hide();

	QSizePolicy retainWidgetSize = ui->informativeCauchyLocation->sizePolicy();
	retainWidgetSize.setRetainSizeWhenHidden(true);
	ui->informativeCauchyLocation->setSizePolicy(retainWidgetSize);

	retainWidgetSize = ui->informativeNormalMean->sizePolicy();
	retainWidgetSize.setRetainSizeWhenHidden(true);
	ui->informativeNormalMean->setSizePolicy(retainWidgetSize);
	ui->informativeNormalMean->hide();

	ui->informativeNormalStd->hide();

	retainWidgetSize = ui->informativeTLocation->sizePolicy();
	retainWidgetSize.setRetainSizeWhenHidden(true);
	ui->informativeTLocation->setSizePolicy(retainWidgetSize);
	ui->informativeTLocation->hide();

	ui->informativeTScale->hide();

	retainWidgetSize = ui->informativeTDf->sizePolicy();
	retainWidgetSize.setRetainSizeWhenHidden(true);
	ui->informativeTDf->setSizePolicy(retainWidgetSize);
	ui->informativeTDf->hide();

	retainWidgetSize = ui->halfNormalDienesStd->sizePolicy();
	retainWidgetSize.setRetainSizeWhenHidden(true);
	ui->halfNormalDienesStd->setSizePolicy(retainWidgetSize);

	retainWidgetSize = ui->normalDienesStd->sizePolicy();
	retainWidgetSize.setRetainSizeWhenHidden(true);
	ui->normalDienesStd->setSizePolicy(retainWidgetSize);

	ui->label_normalDienesMean->hide();
	ui->normalDienesMean->hide();
	ui->label_normalDienesStd->hide();
	ui->normalDienesStd->hide();
	ui->label_uniformDienesLowerBound->hide();
	ui->uniformDienesLowerBound->hide();
	ui->label_uniformDienesUpperBound->hide();
	ui->uniformDienesUpperBound->hide();

#ifdef QT_NO_DEBUG
    ui->_2dienesRawEffectSize->hide();
    ui->line_2->hide();
    ui->dienesEffectSize->hide();
    ui->_1standardizedEffectSize->hide();
#else
    ui->_1standardizedEffectSize->setStyleSheet("background-color: pink;");
    ui->line_2->setStyleSheet("background-color: pink;");
    ui->dienesEffectSize->setStyleSheet("background-color: pink;");
    ui->_2dienesRawEffectSize->setStyleSheet("background-color: pink;");

#endif

}

SubjectivePriorsWidget::~SubjectivePriorsWidget()
{
	delete ui;
}

void SubjectivePriorsWidget::on__1standardizedEffectSize_clicked(bool checked)
{
	ui->effectSizeStandardized->setEnabled(true);
	ui->dienesEffectSize->setEnabled(false);
}

void SubjectivePriorsWidget::on__2dienesRawEffectSize_clicked(bool checked)
{
	ui->dienesEffectSize->setEnabled(true);
	ui->effectSizeStandardized->setEnabled(false);
}

void SubjectivePriorsWidget::on_defaultStandardEffectSize_clicked(bool checked)
{
	if (checked && ui->_1standardizedEffectSize->isChecked())
	{
		ui->defaultStandardizedEffectSize->setEnabled(true);
		ui->informativeStandardizedEffectSize->setEnabled(false);
	}
}

void SubjectivePriorsWidget::on_informativeStandardEffectSize_clicked(bool checked)
{
	if (checked && ui->_1standardizedEffectSize->isChecked())
	{
		ui->informativeStandardizedEffectSize->setEnabled(true);
		ui->defaultStandardizedEffectSize->setEnabled(false);
	}
}

void SubjectivePriorsWidget::on_cauchyInformative_clicked()
{
	ui->label_informativeCauchyLocation->show();
	ui->label_informativeCauchyScale->show();
	ui->informativeCauchyLocation->show();
	ui->informativeCauchyScale->show();

	ui->label_informativeNormalMean->hide();
	ui->label_informativeNormalStd->hide();
	ui->informativeNormalMean->hide();
	ui->informativeNormalStd->hide();

	ui->label_informativeTLocation->hide();
	ui->label_informativeTScale->hide();
	ui->label_informativeTDf->hide();
	ui->informativeTLocation->hide();
	ui->informativeTScale->hide();
	ui->informativeTDf->hide();
}

void SubjectivePriorsWidget::on_normalInformative_clicked()
{
	ui->label_informativeCauchyLocation->hide();
	ui->label_informativeCauchyScale->hide();
	ui->informativeCauchyLocation->hide();
	ui->informativeCauchyScale->hide();

	ui->label_informativeNormalMean->show();
	ui->label_informativeNormalStd->show();
	ui->informativeNormalMean->show();
	ui->informativeNormalStd->show();

	ui->label_informativeTLocation->hide();
	ui->label_informativeTScale->hide();
	ui->label_informativeTDf->hide();
	ui->informativeTLocation->hide();
	ui->informativeTScale->hide();
	ui->informativeTDf->hide();
}

void SubjectivePriorsWidget::on_tInformative_clicked()
{
	ui->label_informativeCauchyLocation->hide();
	ui->label_informativeCauchyScale->hide();
	ui->informativeCauchyLocation->hide();
	ui->informativeCauchyScale->hide();

	ui->label_informativeNormalMean->hide();
	ui->label_informativeNormalStd->hide();
	ui->informativeNormalMean->hide();
	ui->informativeNormalStd->hide();

	ui->label_informativeTLocation->show();
	ui->label_informativeTScale->show();
	ui->label_informativeTDf->show();
	ui->informativeTLocation->show();
	ui->informativeTScale->show();
	ui->informativeTDf->show();
}

void SubjectivePriorsWidget::on_halfNormalDienes_clicked()
{
	ui->label_halfNormalDienesStd->show();
	ui->halfNormalDienesStd->show();

	ui->label_normalDienesMean->hide();
	ui->normalDienesMean->hide();
	ui->label_normalDienesStd->hide();
	ui->normalDienesStd->hide();

	ui->label_uniformDienesLowerBound->hide();
	ui->uniformDienesLowerBound->hide();
	ui->label_uniformDienesUpperBound->hide();
	ui->uniformDienesUpperBound->hide();
}

void SubjectivePriorsWidget::on_normalDienes_clicked()
{
	ui->label_halfNormalDienesStd->hide();
	ui->halfNormalDienesStd->hide();

	ui->label_normalDienesMean->show();
	ui->normalDienesMean->show();
	ui->label_normalDienesStd->show();
	ui->normalDienesStd->show();

	ui->label_uniformDienesLowerBound->hide();
	ui->uniformDienesLowerBound->hide();
	ui->label_uniformDienesUpperBound->hide();
	ui->uniformDienesUpperBound->hide();
}

void SubjectivePriorsWidget::on_uniformDienes_clicked()
{
	ui->label_halfNormalDienesStd->hide();
	ui->halfNormalDienesStd->hide();

	ui->label_normalDienesMean->hide();
	ui->normalDienesMean->hide();
	ui->label_normalDienesStd->hide();
	ui->normalDienesStd->hide();

	ui->label_uniformDienesLowerBound->show();
	ui->uniformDienesLowerBound->show();
	ui->label_uniformDienesUpperBound->show();
	ui->uniformDienesUpperBound->show();
}
