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

#ifndef AVAILABLEFIELDS_H
#define AVAILABLEFIELDS_H

#include <QAbstractListModel>
#include <QStringList>
#include <QIcon>

#include <vector>
#include <string>

#include "dataset.h"
#include "options/optionvariables.h"

class AvailableFields : public QAbstractListModel
{
public:
	AvailableFields(QObject *parent);

	void setDataSet(DataSet *dataSet);
	void filter(std::vector<std::string> show);

	void provideFor(OptionVariables *option);
	QStringList getFields(QModelIndexList indices);

	boost::signals2::signal<void ()> availableFieldsChanged();

	int rowCount(const QModelIndex &) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;

	QStringList available();

private:
	DataSet *_dataSet;
	std::vector<OptionVariables *> _provideFor;
	void updateAvailableFields();
	QStringList _availableFields;

	std::vector<std::string> _filter;
	bool _shouldFilter;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;

};

#endif // AVAILABLEFIELDS_H
