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

#ifndef LISTMODELLABELVALUETERMS_H
#define LISTMODELLABELVALUETERMS_H

#include "controls/jasplistcontrol.h"
#include "listmodelavailableinterface.h"
#include "controls/sourceitem.h"

class ListModelLabelValueTerms : public ListModelAvailableInterface
{
	Q_OBJECT
public:
	ListModelLabelValueTerms(JASPListControl* listView, const SourceItem::SourceValuesType& values = SourceItem::SourceValuesType());

	QVariant					data(const QModelIndex &index, int role = Qt::DisplayRole)	const	override;
	void						resetTermsFromSources()												override;

	std::vector<std::string>	getValues();
	QString						getValue(const QString& label)								const;
	QString						getLabel(const QString& value)								const;
	QString						getInfo(const QString& label)								const;
	int							getIndexOfValue(const QString& value)						const;
	int							getIndexOfLabel(const QString& label)						const;

	void						setLabelValuesFromSource();

public slots:
	void						sourceNamesChanged(QMap<QString, QString> map)					override;

protected:
	void						_setLabelValues(const SourceItem::SourceValuesType& values);

	QMap<QString, int>				_valuesMap;
	QMap<QString, int>				_labelsMap;
	SourceItem::SourceValuesType	_labelValues;

};

#endif // LISTMODELLABELVALUETERMS_H
