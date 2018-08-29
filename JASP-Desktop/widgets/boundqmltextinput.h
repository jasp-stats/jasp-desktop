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

#ifndef BOUNDQMLTEXTINPUT_H
#define BOUNDQMLTEXTINPUT_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optioninteger.h"
#include "analysis/options/optionnumber.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionintegerarray.h"

#include <QObject>


class BoundQMLTextInput : public BoundQMLItem
{
	Q_OBJECT
	
public:
	enum TextInputType { IntegerInputType = 0, StringInputType, NumberInputType, PercentIntputType, IntegerArrayInputType };
	
	explicit BoundQMLTextInput(QQuickItem* item, AnalysisQMLForm* form);
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	
	virtual Option* createOption() OVERRIDE;
	virtual void resetQMLItem(QQuickItem *item) OVERRIDE;

signals:
	
private slots:
	void textChangedSlot();
    
protected:
private:
	void setOptionValue(QString& text);
	
	QString _getPercentValue();
	QString _getIntegerArrayValue();
	
	TextInputType _inputType;
	OptionInteger *_integer;
	OptionIntegerArray *_integerArray;
	OptionNumber *_number;
	OptionString *_string;
	Option *_option;

};

#endif // BOUNDQMLTEXTINPUT_H
