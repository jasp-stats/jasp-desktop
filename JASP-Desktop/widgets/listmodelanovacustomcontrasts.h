//
// Copyright (C) 2013-2019 University of Amsterdam
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

#ifndef LISTMODELANOVACUSTOMCONTRASTS_H
#define LISTMODELANOVACUSTOMCONTRASTS_H

#include "listmodeltableviewbase.h"


class ListModelANOVACustomContrasts : public ListModelTableViewBase
{
	Q_OBJECT
	Q_PROPERTY(QString		colName		READ colName	WRITE setColName	NOTIFY colNameChanged	)

public:
	explicit ListModelANOVACustomContrasts(BoundQMLTableView * parent);

	QString			getColName(size_t index)								const	override;
	int				getMaximumColumnWidthInCharacters(size_t columnIndex)	const	override;
	void			loadColumnInfo();
	void			modelChangedSlot()												override;
	OptionsTable *	createOption()													override;
	void			initValues(OptionsTable * bindHere)								override;
	void			reset()															override;

	QString			colName()												const				{ return _colName;	}

public slots:
	void sourceTermsChanged(Terms* termsAdded, Terms* termsRemoved)					override;
	void setColLabels(QVector<QString> labels);
	void setColName(QString colName);
	void dataSetChangedHandler();


signals:
	void colNameChanged(QString colName);

private:
	void modifyValuesNamesEtcetera();

private:
	QString		_colName;
};

#endif // LISTMODELANOVACUSTOMCONTRASTS_H


