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

#ifndef TABLEMODELANOVAMODELNUISANCEFACTORS_H
#define TABLEMODELANOVAMODELNUISANCEFACTORS_H

#include "tablemodelanovamodel.h"
#include "options/optionvariables.h"

class TableModelAnovaModelNuisanceFactors : public TableModelAnovaModel
{
public:
	TableModelAnovaModelNuisanceFactors(QObject *parent = 0);

	virtual int columnCount(const QModelIndex &parent) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;	
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	void assignToNuisanceOption();
	void setNuisanceTermsOption(OptionVariables *nuisanceOption);

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

protected:
	QList<bool> _nuisance;
	OptionVariables *_nuisanceOption;
};

#endif // TABLEMODELANOVAMODELNUISANCEFACTORS_H
