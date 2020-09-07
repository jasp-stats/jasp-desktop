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

#include "listmodeltableviewsimple.h"
#include "qmllistview.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optiondoublearray.h"

ListModelTableViewSimple::ListModelTableViewSimple(BoundQMLTableView * parent, QString tableType)
    : ListModelTableViewBase(parent, tableType)
{
    _keepRowsOnReset	= true;
    _defaultCellVal     = 1;
}


OptionsTable* ListModelTableViewSimple::createOption()
{
    OptionsTable* result = ListModelTableViewBase::createOption();

    QVariant modelVar = listView()->getItemProperty("values");
    QList<QVariant> list = modelVar.toList();

    std::vector<std::string>    levels;
    std::vector<double>         values;

    for (const QVariant& itemVariant : list)
    {
        levels.push_back(itemVariant.toString().toStdString());
        values.push_back(_defaultCellVal.toDouble());
    }

	std::vector<Options*> allOptions;
	for (size_t i = 0; i < _initialColCnt; i++)
	{
		Options* options = new Options();
		options->add("levels", new OptionVariables(levels));
		options->add("name", new OptionString(getDefaultColName(0).toStdString()));
		options->add("values", new OptionDoubleArray(values));
		allOptions.push_back(options);
	}

    result->setValue(allOptions);

	return result;
}

QString ListModelTableViewSimple::getDefaultColName(size_t index) const
{
	return listView()->getItemProperty("colName").toString() + " " + QString::number(index + 1);
}

