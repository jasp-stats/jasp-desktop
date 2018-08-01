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

#ifndef BOUNDTEXTBOX_H
#define BOUNDTEXTBOX_H

#include <QLineEdit>
#include <QValidator>

#include "analysis/options/optioninteger.h"
#include "analysis/options/optionintegerarray.h"
#include "analysis/options/optionnumber.h"
#include "analysis/options/optionstring.h"

#include "analysis/options/bound.h"

class BoundTextBox : public QLineEdit, public Bound
{
	Q_OBJECT
public:
	explicit BoundTextBox(QWidget *parent = 0);

	void bindTo(Option *option)				override;
	void setLabel(const QString &label);
    void finalise();
	
protected:
	void keyPressEvent(QKeyEvent *event)	override;
	void focusOutEvent(QFocusEvent *event)	override;

private:
	OptionInteger		*_integer;
	OptionIntegerArray	*_integerArray;
	OptionNumber		*_number;
	OptionString		*_string;

	QString				_label;

private slots:
	void textEditedHandler(QString text);

private:
	class QIntArrayValidator : public QValidator
	{
	public:
									QIntArrayValidator();
				QValidator::State	validate(QString & input, int&pos)	const override;
				void				fixup(QString &input)				const override;

		static	std::vector<int>	parse(QString &input);
		static	QString				stringify(std::vector<int> &input);

	};
};

#endif // BOUNDTEXTBOX_H
