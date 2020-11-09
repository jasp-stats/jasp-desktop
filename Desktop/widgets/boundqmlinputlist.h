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

#ifndef BOUNDQMLINPUTLIST_H
#define BOUNDQMLINPUTLIST_H

#include "analysis/boundqmlitem.h"
#include "listmodelinputvalue.h"
#include "qmllistview.h"
#include "analysis/options/optionstable.h"

class BoundQMLInputList : public QMLListView, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLInputList(JASPControl* item);

	ListModel*	model()								const	override { return _inputModel; }
	Option*		boundTo()									override { return _boundTo; }
	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue)	override;

protected slots:
	void		modelChangedHandler()						override;

private:
	ListModelInputValue*		_inputModel			= nullptr;
	OptionsTable*				_boundTo			= nullptr;
	std::vector<std::string>	_defaultValues;
	
};

#endif // BOUNDQMLINPUTLIST_H
