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
#include "listmodeljagsdatainput.h"
#include "analysis/analysisform.h"
#include "r_functionwhitelist.h"
#include "tableviewbase.h"

ListModelJAGSDataInput::ListModelJAGSDataInput(TableViewBase *parent, QString tableType) : ListModelTableViewBase(parent, tableType)
{
	_keepRowsOnReset = false;
	_tableTerms.colNames.push_back(getDefaultColName(0));
	_tableTerms.values.push_back({});
	_tableTerms.colNames.push_back(getDefaultColName(1));
	_tableTerms.values.push_back({});

	parent->setProperty("parseDefaultValue", false);
}

void ListModelJAGSDataInput::sourceTermsReset()
{
	beginResetModel();

	Terms sourceTerms = getSourceTerms();
	if (_tableTerms.values.length() > 0)
	{
		QMap<QString, QVariant> mapping;
		const QVector<QVariant> & firstCol  = _tableTerms.values[0],
								& secondCol = _tableTerms.values[1];
		int row = 0;

		for (const QVariant& key : firstCol)
		{
			mapping[key.toString()] = secondCol[row];
			row++;
		}

		_tableTerms.values.clear();
		_tableTerms.rowNames.clear();
		size_t rowCount = sourceTerms.size();

		for (size_t row = 1; row <= rowCount; row++)
			_tableTerms.rowNames.push_back(getDefaultRowName(row));

		QList<QString>		firstColumnValues = sourceTerms.asQList();
		QVector<QVariant>	firstColumn,
							secondColumn;

		for (const QString& firstValue : firstColumnValues)
		{
			firstColumn.push_back(firstValue);
			QVariant secondValue = mapping.contains(firstValue) ? mapping[firstValue] : _tableView->defaultEmptyValue();
			secondColumn.push_back(secondValue);
		}

		_tableTerms.values.push_back(firstColumn);
		_tableTerms.values.push_back(secondColumn);
	}

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}


QString ListModelJAGSDataInput::getDefaultColName(size_t index) const
{
	if(index == 0)
		return "Parameter";
	return "R Code";
}

int ListModelJAGSDataInput::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	return columnIndex == 0 ? 6 : 25;
}
