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
	QML_ELEMENT

public:
	InputListBase(QQuickItem* parent = nullptr);

	bool			isJsonValid(const Json::Value& optionValue)	const	override;
	Json::Value		createJson()								const	override;
	void			bindTo(const Json::Value& value)					override;
	ListModel*		model()										const	override { return _inputModel; }
	void			setUpModel()										override;

signals:
	void			itemChanged(int index, QString name);
	void			itemRemoved(int index);

protected slots:
	void			termsChangedHandler()								override;

protected:
	// live input-model terms are the written value (termsChangedHandler writes
	//exactly this); createJson stays the defaultValues builder for the initial binding.
	//The sidecar is the positional per-term array (same schema as BoundControlTerms).
	Json::Value		createEngineValue()												const	override;
	// row-bearing input lists store the per-term objects under the optionKey
	//({name, filterId?, types}, row children kept); non-row input lists keep the plain string
	//array in the file too (their values are free text, and the card's three reshaped families
	//are exactly the Terms::getOptions serializations - a non-row input list never builds one).
	Json::Value		createStorageValue()												const	override;
	Json::Value		createFilterIds()													const	override;

private:
	ListModelInputValue*		_inputModel			= nullptr;
	std::vector<std::string>	_defaultValues;
	
};

#endif // INPUTLISTBASE_H
