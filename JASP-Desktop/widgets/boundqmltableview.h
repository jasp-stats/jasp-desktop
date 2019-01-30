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

#ifndef BOUNDQMLTABLEVIEW_H
#define BOUNDQMLTABLEVIEW_H

#include "analysis/boundqmlitem.h"
#include "qmllistview.h"

#include "listmodelmultinomialchi2test.h"

#include "analysis/options/optionstable.h"
#include <QObject>

class BoundQMLTableView : public QMLListView, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLTableView(QQuickItem* item, AnalysisForm* form);
	virtual void bindTo(Option *option) OVERRIDE;
	
	virtual ListModel* model() OVERRIDE		{ return _tableModel; }
	
	virtual Option* createOption() OVERRIDE;
	virtual bool isOptionValid(Option* option) OVERRIDE;	
	virtual Option* boundTo() OVERRIDE { return _boundTo; }
	virtual void setUp() OVERRIDE;

    
protected:
	OptionsTable *_boundTo;
	ListModel *_tableModel = nullptr;
	ListModelMultinomialChi2Test *_multinomialChi2TestModel = nullptr;
	
private slots:
	void addColumnSlot();
	void removeColumnSlot(int col);
	void resetSlot();
	void itemChangedSlot(int col, int row, QString value);
	void modelChangedSlot();

};

#endif // BOUNDQMLTABLEVIEW_H
