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

#ifndef ANOVAMODELWIDGET_H
#define ANOVAMODELWIDGET_H

#include <QWidget>
#include <QStringListModel>
#include <QItemDelegate>
#include <QPainter>
#include <QToolTip>

#include "bound.h"

#include "options/optionvariables.h"
#include "availablefields.h"

#include "tablemodelvariablesavailable.h"
#include "tablemodelanovamodel.h"


class AnovaHoverDelegate : public QItemDelegate
{
public:
	AnovaHoverDelegate(QObject *parent=0) : QItemDelegate(parent){}
	void paint ( QPainter *painter, const QStyleOptionViewItem & option, const QModelIndex & index ) const
	{
		if(option.state & QStyle::State_MouseOver)
		{
			painter->fillRect(option.rect, Qt::lightGray);
			QString str = index.data().toString();
			QToolTip::showText(QCursor::pos(), str);
		}
		QItemDelegate::paint(painter, option, index);
	}
};

namespace Ui {
class AnovaModelWidget;
}

class AnovaModelWidget : public QWidget, public Bound
{
	Q_OBJECT
	
public:
	explicit AnovaModelWidget(QWidget *parent = 0);
	~AnovaModelWidget();

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void setModel(TableModelAnovaModel *model);

	void setFactorsLabel(const QString &label);
	
private slots:

	void variablesAvailableChanged();
	void sourceSelectionChanged();

	void assignInteraction();
	void assignMainEffects();
	void assign2ways();
	void assign3ways();
	void assign4ways();
	void assign5ways();

private:

	QAction *_assignInteraction;
	QAction *_assignMainEffects;
	QAction *_assign2ways;
	QAction *_assign3ways;
	QAction *_assign4ways;
	QAction *_assign5ways;

	Ui::AnovaModelWidget *ui;

	OptionsTable *_boundTo;

	TableModelVariablesAvailable *_tableModelVariablesAvailable;
	TableModelAnovaModel *_tableModelAnovaModel;

};

#endif // ANOVAMODELWIDGET_H
