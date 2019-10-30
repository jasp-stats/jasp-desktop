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

#include "log.h"
#include "utilities/qutils.h"
#include "listmodelmultinomialchi2test.h"
#include "analysis/analysisform.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optiondoublearray.h"

void ListModelMultinomialChi2Test::sourceTermsChanged(Terms *termsAdded, Terms *)
{
	beginResetModel();

	_rowNames.clear();
	_colNames.clear();
	_values.clear();
	_columnCount = 0;

	if (termsAdded && termsAdded->size() > 0)
	{
		const std::string	& colName	= termsAdded->at(0).asString();
		QStringList			  labels	= listView()->form()->getDataSetPackage()->getColumnLabelsAsStringList(colName);

		_rowNames = labels.toVector();

		QVector<QVariant> newValues(_rowNames.length(), 1.0);
		_values.push_back(newValues);
		_colNames.push_back(getColName(0));
		_columnCount = 1;

	}

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
	emit modelChanged();
}


QString ListModelMultinomialChi2Test::getColName(size_t index) const
{
	if (_tableType == "PriorCounts")
		return "Counts";

	if (index >= _maxColumn)
		index = _maxColumn - 1;

	char letter = char(97 + index);
	return tq("H₀ (") + letter + tq(")");
}



