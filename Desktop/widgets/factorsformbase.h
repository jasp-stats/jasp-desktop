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

#ifndef FACTORSFORMBASE_H
#define FACTORSFORMBASE_H

#include "jasplistcontrol.h"
#include "analysis/options/boundcontrol.h"
#include "analysis/options/optionstable.h"
#include "listmodelfactorsform.h"


class FactorsFormBase :  public JASPListControl, public BoundControl
{
	Q_OBJECT
	
public:
	FactorsFormBase(QQuickItem* parent = nullptr);

	ListModel*	model()								const	override { return _factorsModel; }
	void		setUpModel()								override;
	Option*		boundTo()									override { return _boundTo; }
	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;

protected slots:
	void modelChangedHandler() override;
	void addListViewSlot(JASPListControl* listView);
	
private:
	ListModelFactorsForm*	_factorsModel				= nullptr;
	OptionsTable*			_boundTo					= nullptr;
	QString					_availableVariablesListName;
	JASPControl*			_availableVariablesListItem	= nullptr;
	int						_initNumberFactors			= 1;
};

#endif // FACTORSFORMBASE_H
