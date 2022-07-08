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
#include "analysisform.h"
#include "controls/tableviewbase.h"

ListModelMultinomialChi2Test::ListModelMultinomialChi2Test(TableViewBase * parent)
	: ListModelTableViewBase(parent)
{
	_tableView->setUseSourceLevels(true);
}

bool ListModelMultinomialChi2Test::sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)
{
	if (!_columnsUsedForLabels.contains(columnName))
		return false;

	if (changedLabels.size() == 0)
	{
		// the changed labels are not specified. Reset the values from the source.
		sourceTermsReset();
		return true;
	}

	QList<QString> keys = changedLabels.keys();
	for (int row=0; row<_tableTerms.rowNames.size(); row++)
		if (changedLabels.contains(_tableTerms.rowNames[row]))
		{
			_tableTerms.rowNames[row] = changedLabels[_tableTerms.rowNames[row]];
			emit headerDataChanged(Qt::Vertical, row, row);
			break;
		}

	return false;
}

bool ListModelMultinomialChi2Test::sourceLabelsReordered(QString columnName)
{
	if (!_columnsUsedForLabels.contains(columnName))
		return false;

	std::map<QString, std::vector<QVariant>> tempStore;

	//because everything is stored in columns we first need to map all the rows, to well rows (with the name being key)
	for (int row = 0; row < rowCount(); row++)
		for (int col = 0; col < columnCount(); col++)
			tempStore[_tableTerms.rowNames[row]].push_back(_tableTerms.values[col][row]);

	beginResetModel();
	_tableTerms.rowNames = getSourceTerms().asQList();
	_tableTerms.values.clear();
	_tableTerms.values.resize(columnCount());

	for (int row = 0; row < rowCount(); row++)
		for( int col = 0; col < columnCount(); col++)
			_tableTerms.values[col].push_back(tempStore[_tableTerms.rowNames[row]][size_t(col)]);

	endResetModel();

	return true;
}

