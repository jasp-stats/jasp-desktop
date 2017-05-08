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

#ifndef SUBJECTIVEPRIORSWIDGET_H
#define SUBJECTIVEPRIORSWIDGET_H

#include <QWidget>


namespace Ui {
class SubjectivePriorsWidget;
}

class SubjectivePriorsWidget : public QWidget
{
	Q_OBJECT

public:
	explicit SubjectivePriorsWidget(QWidget *parent = 0);
	~SubjectivePriorsWidget();

private slots:
	void on__1standardizedEffectSize_clicked(bool checked);
	void on__2dienesRawEffectSize_clicked(bool checked);
	void on_defaultStandardEffectSize_clicked(bool checked);
	void on_informativeStandardEffectSize_clicked(bool checked);
	void on_cauchyInformative_clicked();
	void on_normalInformative_clicked();
	void on_tInformative_clicked();
	void on_halfNormalDienes_clicked();
	void on_normalDienes_clicked();
	void on_uniformDienes_clicked();

private:
	Ui::SubjectivePriorsWidget *ui;
};

#endif // SUBJECTIVEPRIORSWIDGET_H
