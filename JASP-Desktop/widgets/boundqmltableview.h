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

#include "boundqmlitem.h"
#include "options/optionstable.h"
#include "listmodel.h"
#include "listmodeltable.h"
#include <QObject>

class BoundQMLTableView : public BoundQMLItem
{
	Q_OBJECT
	
public:
	explicit BoundQMLTableView(QQuickItem* item, AnalysisQMLForm* form);
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	
	virtual Option* createOption() OVERRIDE;
	
	virtual void setUp() OVERRIDE;	

private slots:
	virtual void syncTermsChanged(Terms* termsAdded, Terms* termsRemoved);
	virtual void comboBoxActivatedHandler(int row, QString value);
	
    
protected:
	OptionsTable *_boundTo;
	std::vector<ListModel*> _syncModels;
	ListModelTable* _tableModel;
	Terms _terms;
	
	virtual void resetTermsFromSyncModels();	
};

#endif // BOUNDQMLTABLEVIEW_H
