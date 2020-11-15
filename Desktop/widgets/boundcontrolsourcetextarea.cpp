//
// Copyright (C) 2013-2020 University of Amsterdam
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

#include "boundcontrolsourcetextarea.h"
#include "textareabase.h"

void BoundControlSourceTextArea::bindTo(Option *option)
{
	BoundControlTextArea::bindTo(option);
	_setSourceTerms();
}

void BoundControlSourceTextArea::checkSyntax()
{
	BoundControlTextArea::checkSyntax();
	_setSourceTerms();
}

void BoundControlSourceTextArea::_setSourceTerms()
{
	QStringList list = {_textArea->text()};
	for (const QString& separator : _textArea->separators())
	{
		QStringList newList;
		for (const QString& listPart : list)
			newList.append(listPart.split(separator, QString::SkipEmptyParts));
		list = newList;
	}

	QStringList terms;

	for (const QString& term : list)
		terms.append(term.trimmed());
	_textArea->model()->initTerms(terms);

	emit _textArea->model()->modelChanged(nullptr, nullptr);
}
