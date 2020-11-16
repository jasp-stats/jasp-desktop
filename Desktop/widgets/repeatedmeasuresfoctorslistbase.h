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

#ifndef REPEATEDMEASURESFACTORSLISTBASE_H
#define REPEATEDMEASURESFACTORSLISTBASE_H

#include "jasplistcontrol.h"
#include "analysis/options/boundcontrol.h"
#include "listmodelrepeatedmeasuresfactors.h"
#include "analysis/options/optionstable.h"

class RepeatedMeasuresFoctorsListBase :  public JASPListControl, public BoundControl
{
	Q_OBJECT
	
public:
	RepeatedMeasuresFoctorsListBase(QQuickItem* parent = nullptr);

	ListModel*	model()								const	override { return _factorsModel; }
	void		setUpModel()								override;
	void		setUp()										override;
	Option*		boundTo()									override { return _boundTo; }
	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;

protected slots:
	void termsChangedHandler() override;
	
private:
	ListModelRepeatedMeasuresFactors*	_factorsModel	= nullptr;
	OptionsTable*						_boundTo		= nullptr;
	
};

#endif // REPEATEDMEASURESFACTORSLISTBASE_H
