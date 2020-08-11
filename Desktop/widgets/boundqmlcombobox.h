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

#ifndef BOUNDQMLCOMBOBOX_H
#define BOUNDQMLCOMBOBOX_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optionlist.h"
#include "listmodel.h"
#include "listmodellabelvalueterms.h"
#include <QMap>

class BoundQMLComboBox : public QMLListView, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLComboBox(JASPControlBase* item);

	void		bindTo(Option *option)						override;
	void		resetQMLItem(JASPControlBase *item)			override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;	
	bool		isJsonValid(const Json::Value& optionValue) override;
	Option*		boundTo()									override	{ return _boundTo; }
	void		setUp()										override;
	ListModel*	model()								const	override	{ return _model; }
	
protected slots:
	void modelChangedHandler() override;
	void comboBoxChangeValueSlot(int index);
	void languageChangedHandler();

protected:
	OptionList*					_boundTo				= nullptr;
	int							_currentIndex			= 0;
	QString						_currentText;
	QString						_currentColumnType;
	ListModelLabelValueTerms*	_model					= nullptr;

	void _setLabelValues();
	void _resetItemWidth();
	void _resetOptions();
	void _setCurrentValue(int index, bool setComboBoxIndex = true, bool setOption = true);
};

#endif // BOUNDQMLCOMBOBOX_H
