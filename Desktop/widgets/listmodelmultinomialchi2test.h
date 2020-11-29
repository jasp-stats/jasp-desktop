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

#ifndef LISTMODELMULTINOMIALCHI2TEST_H
#define LISTMODELMULTINOMIALCHI2TEST_H

#include "listmodeltableviewbase.h"


class ListModelMultinomialChi2Test : public ListModelTableViewBase
{
	Q_OBJECT

public:
	explicit ListModelMultinomialChi2Test(TableViewBase * parent, QString tableType);

	QString	getDefaultColName(size_t index) const	override;

public slots:
	void sourceTermsChanged()						override;
	void labelChanged(	 QString columnName, QString originalLabel, QString newLabel);
	void labelsReordered(QString columnName);

private:
	QString _columnBeingTracked = "";
};

#endif // LISTMODELMULTINOMIALCHI2TEST_H

