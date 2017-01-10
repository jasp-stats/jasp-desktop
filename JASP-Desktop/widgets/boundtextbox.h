//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionnumber.h"

#include "bound.h"

class BoundTextBox : public QLineEdit, public Bound
{
	Q_OBJECT
public:
	explicit BoundTextBox(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;

	void setLabel(const QString &label);
    void finalise();
signals:
	
protected:
	void keyPressEvent(QKeyEvent *event) OVERRIDE;
	void focusOutEvent(QFocusEvent *event) OVERRIDE;

private:
	OptionInteger *_integer;
	OptionIntegerArray *_integerArray;
	OptionNumber *_number;

	QString _label;

private slots:
	void textEditedHandler(QString text);

private:
	class QIntArrayValidator : public QValidator
	{
	public:
		QIntArrayValidator();
		QValidator::State validate(QString & input, int&pos) const OVERRIDE;
		virtual void fixup(QString &input) const OVERRIDE;

		static std::vector<int> parse(QString &input);
		static QString stringify(std::vector<int> &input);

	};
	
};

#endif // BOUNDTEXTBOX_H
