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

#include "jasplistcontrol.h"
#include "listmodelavailableinterface.h"

class ListModelLabelValueTerms : public ListModelAvailableInterface
{
	Q_OBJECT
public:
	ListModelLabelValueTerms(JASPListControl* listView, const JASPListControl::LabelValueMap& values = JASPListControl::LabelValueMap());

	QVariant					data(const QModelIndex &index, int role = Qt::DisplayRole)	const	override;
	void						resetTermsFromSources(bool updateAssigned = true)				override;

	std::vector<std::string>	getValues();
	QString						getValue(const QString& label);
	QString						getLabel(const QString& value);
	int							getIndexOfValue(const QString& value);

	void						setLabelValuesFromSource();

public slots:
	void						sourceNamesChanged(QMap<QString, QString> map)					override;

protected:
	void						_setLabelValues(const JASPListControl::LabelValueMap& values);

	QMap<QString, QString>		_valueToLabelMap;
	QMap<QString, QString>		_labelToValueMap;

};

#endif // LISTMODELLABELVALUETERMS_H
