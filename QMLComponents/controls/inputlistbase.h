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

#ifndef INPUTLISTBASE_H
#define INPUTLISTBASE_H

#include "jasplistcontrol.h"
#include "boundcontrols/boundcontrolbase.h"
#include "models/listmodelinputvalue.h"

class InputListBase : public JASPListControl, public BoundControlBase
{
	Q_OBJECT
	
public:
	InputListBase(QQuickItem* parent = nullptr);

	bool			isJsonValid(const Json::Value& optionValue)	override;
	Json::Value		createJson()								override;
	void			bindTo(const Json::Value& value)			override;
	ListModel*		model()								const	override { return _inputModel; }
	void			setUpModel()								override;

signals:
	void			itemChanged(int index, QString name);
	void			itemRemoved(int index);

protected slots:
	void			termsChangedHandler()						override;

private:
	ListModelInputValue*		_inputModel			= nullptr;
	std::vector<std::string>	_defaultValues;
	
};

#endif // INPUTLISTBASE_H
