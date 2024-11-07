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

#ifndef BOUNDQMLLISTVIEWTERMS_H
#define BOUNDQMLLISTVIEWTERMS_H

#include "boundcontrolbase.h"
#include "models/listmodelassignedinterface.h"

class BoundControlTerms : public BoundControlBase
{
	
public:
	BoundControlTerms(ListModelAssignedInterface* listModel, bool isSingleRow = false);
	
	bool		isJsonValid(const Json::Value& value)										const	override;
	Json::Value	createJson()																const	override;
	void		bindTo(const Json::Value &value)													override;
	void		resetBoundValue()																	override;
	void		setBoundValue(const Json::Value &value, bool emitChanges = true)					override;

	Json::Value	addTermsToOption(const Json::Value &option, const Terms &terms, const ListModel::RowControlsValues &extraTermsMap = {}) const;
	bool		areTermsInOption(const Json::Value& option,	Terms& terms)					const;

	static Json::Value makeOption(const Terms& terms, const ListModel::RowControlsValues& controlValues, const std::string& optionKey, bool containsInteractions, bool hasRowComponent, bool isSingleRow);

private:
	Terms		_getTermsFromOptions(const Json::Value& option)								const;
	Json::Value	_adjustBindingValue(const Json::Value &value)								const;
	Json::Value	_adjustBindingType(const Json::Value &value)								const;
	Json::Value _makeOption(const Terms& terms, const ListModel::RowControlsValues& controlValues) const;

	ListModelAssignedInterface*		_termsModel				= nullptr;
	JASPListControl*				_listView				= nullptr;
	bool							_isSingleRow			= false;
	std::string						_optionKey;
};

#endif // BOUNDQMLLISTVIEWTERMS_H
