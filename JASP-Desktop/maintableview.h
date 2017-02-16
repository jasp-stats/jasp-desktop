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

#ifndef TABLEVIEW_H
#define TABLEVIEW_H

#include <QTableView>

#include "widgets/infopopup.h"

#include "datasettablemodel.h"
#include "maintablehorizontalheader.h"
#include "variableswidget.h"

#include <QPushButton>

class MainTableView : public QTableView
{
	Q_OBJECT

public:
	explicit MainTableView(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model) OVERRIDE;
	void setVariablesView(VariablesWidget *variablesPage);
	void adjustAfterDataLoad(bool dataLoaded);

protected:
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) OVERRIDE;
	virtual void verticalScrollbarValueChanged(int value) OVERRIDE;
	virtual void mouseDoubleClickEvent(QMouseEvent *event) OVERRIDE;

signals:
	void dataTableColumnSelected();
	void dataTableDoubleClicked();
	
public slots:

private slots:
	void badDataEnteredHandler(QModelIndex index);
	void columnTypeChanged(int columnIndex, Column::ColumnType newColumnType);
	void showLabelView(int columnIndex);

private:
	DataSetTableModel *_dataSetModel;
	VariablesWidget *_variablesPage;


	bool _infoPopupVisible;
	QModelIndex _infoPopupIndex;
	InfoPopup *_infoPopup;

	void showInfoPopup(QModelIndex &index);
	void moveInfoPopup();
	void hideInfoPopup();

	MainTableHorizontalHeader *_header;
	bool _dataLoaded;
	
};

#endif // TABLEVIEW_H
