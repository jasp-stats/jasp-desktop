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

#ifndef BOUNDQMLLISTVIEWLAYERS_H
#define BOUNDQMLLISTVIEWLAYERS_H

#include "boundqmllistviewdraggable.h"
#include "listmodellayersassigned.h"
#include "analysis/options/optionstable.h"

class BoundQMLListViewLayers : public BoundQMLListViewDraggable
{
	Q_OBJECT
	
public:
	BoundQMLListViewLayers(JASPControlBase* item);
	
	ListModel*	model()								const	override { return _layersModel; }
	Option*		boundTo()									override { return _boundTo; }
	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;

protected slots:
	void modelChangedHandler() override;
	
private:
	OptionsTable*				_boundTo		= nullptr;
	ListModelLayersAssigned*	_layersModel	= nullptr;
	
};

#endif // BOUNDQMLLISTVIEWLAYERS_H
