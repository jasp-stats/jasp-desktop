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

#include "listmodellabelvalueterms.h"
#include "log.h"

ListModelLabelValueTerms::ListModelLabelValueTerms(JASPListControl* listView, const JASPListControl::LabelValueMap& values)
	: ListModelTermsAvailable(listView)
{
	if (listView->property("addEmptyValue").toBool())
		addEmptyValue();

	setLabelValues(values);
}

QVariant ListModelLabelValueTerms::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	const Terms& myTerms = terms();
	size_t row_t = size_t(row);

	if (row < 0 || row_t >= myTerms.size())
		return QVariant();

	if (role == ListModel::ValueRole)
	{
		QString label = myTerms.at(row_t).asQString();
		if (_labelToValueMap.contains(label))
			return _labelToValueMap[label];
		else
			return label;
	}

	return ListModelTermsAvailable::data(index, role);
}


std::vector<std::string> ListModelLabelValueTerms::getValues()
{
	std::vector<std::string> values;
	const Terms& terms = this->terms();
	for (const Term& term : terms)
	{
		QString label = term.asQString();
		QString value = _labelToValueMap.contains(label) ? _labelToValueMap[label] : label;
		values.push_back(value.toStdString());
	}

	return values;
}

QString ListModelLabelValueTerms::getValue(const QString &label)
{
	return _labelToValueMap.contains(label) ? _labelToValueMap[label] : label;
}

QString ListModelLabelValueTerms::getLabel(const QString &value)
{
	return _valueToLabelMap.contains(value) ? _valueToLabelMap[value] : value;
}


int ListModelLabelValueTerms::getIndexOfValue(const QString &value)
{
	int index = 0;
	QString label = getLabel(value);
	for (const Term& term : _terms)
	{
		if (term.asQString() == label)
			return index;
		index++;
	}

	return -1;
}

void ListModelLabelValueTerms::setLabelValues(const JASPListControl::LabelValueMap &labelvalues)
{
	_valueToLabelMap.clear();
	_labelToValueMap = labelvalues;
	_terms.clear();

	QMapIterator<QString, QString> it(labelvalues);
	while (it.hasNext())
	{
		it.next();
		const QString& label = it.key();
		const QString& value = it.value();
		_terms.add(label);
		_valueToLabelMap[value] = label;
	}
}

