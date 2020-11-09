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

#include <QObject>
#include "qmllistview.h"
#include "analysis/boundqmlitem.h"
#include "listmodeltableviewbase.h"
#include "analysis/options/optionstable.h"

class BoundQMLTableView : public QMLListView, public BoundQMLItem
{
	Q_OBJECT

public:
	BoundQMLTableView(JASPControl* item);

	void				bindTo(Option *option)							override;
	ListModel*			model()									const	override { return _tableModel; }
	Option*				createOption()									override;
	bool				isOptionValid(Option * option)					override;
	bool				isJsonValid(const Json::Value & optionValue)	override;
	Option*				boundTo()										override { return _boundTo; }
	void				setUp()											override;
	void				rScriptDoneHandler(const QString & result)		override;

public slots:
	void		refreshMe();

protected:
	OptionsTable				* _boundTo		= nullptr;
	ListModelTableViewBase		* _tableModel	= nullptr;
	
private slots:
	void addColumnSlot();
	void removeColumnSlot(int col);
	void addRowSlot();
	void removeRowSlot(int row);
	void resetSlot();
	void itemChangedSlot(int col, int row, QString value, QString type);
};

#endif // BOUNDQMLTABLEVIEW_H
