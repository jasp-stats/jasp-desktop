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
#include "sourceitem.h"

ListModelLabelValueTerms::ListModelLabelValueTerms(JASPListControl* listView, const JASPListControl::LabelValueMap& values)
	: ListModelAvailableInterface(listView)
{
	_setLabelValues(values);
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

	return ListModelAvailableInterface::data(index, role);
}

void ListModelLabelValueTerms::resetTermsFromSources(bool )
{
	beginResetModel();

	setLabelValuesFromSource();

	endResetModel();
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
	for (const Term& term : terms())
	{
		if (term.asQString() == label)
			return index;
		index++;
	}

	return -1;
}

void ListModelLabelValueTerms::_setLabelValues(const JASPListControl::LabelValueMap &labelvalues)
{
	_valueToLabelMap.clear();
	_labelToValueMap.clear();
	Terms newTerms;

	for (const auto& labelValue :labelvalues)
	{
		const QString& label = labelValue.first;
		const QString& value = labelValue.second;
		newTerms.add(label);
		_valueToLabelMap[value] = label;
		_labelToValueMap[label] = value;
	}

	_setTerms(newTerms);
}

void ListModelLabelValueTerms::setLabelValuesFromSource()
{
	JASPListControl::LabelValueMap labelValuePairs;

	if (listView()->addEmptyValue())
		labelValuePairs.push_back(std::make_pair(listView()->placeholderText(), ""));

	listView()->applyToAllSources([&](SourceItem *sourceItem, const Terms& terms)
	{
		ListModelLabelValueTerms* labelValueSourceModel = qobject_cast<ListModelLabelValueTerms*>(sourceItem->listModel());
		for (const Term& term : terms)
		{
			QString label = term.asQString();
			QString value = labelValueSourceModel ? labelValueSourceModel->getValue(label) : label;
			labelValuePairs.push_back(std::make_pair(label, value));
		}
	});

	_setLabelValues(labelValuePairs);
}

void ListModelLabelValueTerms::sourceNamesChanged(QMap<QString, QString> map)
{
	QMap<QString, QString>	changedNamesMap;
	QSet<int>				changedIndexes;

	QMapIterator<QString, QString> it(map);
	while (it.hasNext())
	{
		it.next();
		const QString& oldName = it.key(), newName = it.value();
		Terms orgTerms = terms();
		QSet<int> indexes = orgTerms.replaceVariableName(oldName.toStdString(), newName.toStdString());
		if (indexes.size() > 0)
		{
			QString oldValue = _labelToValueMap[oldName];
			_labelToValueMap.remove(oldName);
			_valueToLabelMap.remove(oldValue);
			QString newValue = oldValue;
			listView()->applyToAllSources([&](SourceItem *sourceItem, const Terms& terms)
			{
				ListModelLabelValueTerms* labelValueSourceModel = qobject_cast<ListModelLabelValueTerms*>(sourceItem->listModel());
				if (terms.contains(newName) && labelValueSourceModel)
					newValue = labelValueSourceModel->getValue(newName);
			});
			_labelToValueMap[newName] = newValue;
			_valueToLabelMap[newValue] = newName;
			changedIndexes += indexes;
			changedNamesMap[oldName] = newName;
		}
	}

	for (int i : changedIndexes)
	{
		QModelIndex ind = index(i, 0);
		emit dataChanged(ind, ind);
	}

	if (changedNamesMap.size() > 0)
		emit namesChanged(changedNamesMap);
}

