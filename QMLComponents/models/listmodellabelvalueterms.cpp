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
#include "controls/sourceitem.h"

ListModelLabelValueTerms::ListModelLabelValueTerms(JASPListControl* listView, const SourceItem::SourceValuesType& values)
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
		if (_labelsMap.contains(label))
			return _labelValues[_labelsMap[label]].value;
		else
			return label;
	}

	return ListModelAvailableInterface::data(index, role);
}

void ListModelLabelValueTerms::resetTermsFromSources()
{
	beginResetModel();

	setLabelValuesFromSource();

	endResetModel();

	_connectAllSourcesControls();
}


std::vector<std::string> ListModelLabelValueTerms::getValues()
{
	std::vector<std::string> values;
	const Terms& terms = this->terms();
	for (const Term& term : terms)
	{
		QString label = term.asQString();
		QString value = _labelsMap.contains(label) ? _labelValues[_labelsMap[label]].value : label;
		values.push_back(value.toStdString());
	}

	return values;
}

QString ListModelLabelValueTerms::getValue(const QString &label) const
{
	return _labelsMap.contains(label) ? _labelValues[_labelsMap[label]].value : label;
}

QString ListModelLabelValueTerms::getInfo(const QString &label) const
{
	return _labelsMap.contains(label) ? _labelValues[_labelsMap[label]].info : "";
}


QString ListModelLabelValueTerms::getLabel(const QString &value) const
{
	return _valuesMap.contains(value) ? _labelValues[_valuesMap[value]].label : value;
}

int ListModelLabelValueTerms::getIndexOfValue(const QString &value) const
{	
	return terms().indexOf(getLabel(value));
}

int ListModelLabelValueTerms::getIndexOfLabel(const QString &label) const
{
	return terms().indexOf(label);
}

void ListModelLabelValueTerms::_setLabelValues(const SourceItem::SourceValuesType &labelvalues)
{
	_labelValues = labelvalues;
	_valuesMap.clear();
	_labelsMap.clear();
	Terms newTerms;

	int i = 0;
	for (const auto& labelValue : labelvalues)
	{
		const QString& label = labelValue.label;
		const QString& value = labelValue.value;
		newTerms.add(Term::readTerm(label)); // The string can be an interaction between different variables (eg: a * b)
		_valuesMap[value] = i;
		_labelsMap[label] = i;
		i++;
	}

	_setTerms(newTerms);
}

void ListModelLabelValueTerms::setLabelValuesFromSource()
{
	SourceItem::SourceValuesType labelValuePairs;

	if (listView()->addEmptyValue())
		labelValuePairs.push_back(SourceItem::SourceValuesItem(listView()->placeholderText(), "", ""));

	listView()->applyToAllSources([&](SourceItem *sourceItem, const Terms& terms)
	{
		ListModelLabelValueTerms* labelValueSourceModel = qobject_cast<ListModelLabelValueTerms*>(sourceItem->sourceListModel());
		for (const Term& term : terms)
		{
			QString label = term.asQString();
			QString value = labelValueSourceModel ? labelValueSourceModel->getValue(label) : label;
			labelValuePairs.push_back(SourceItem::SourceValuesItem(label, value, ""));
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
		Terms newTerms = terms();
		QSet<int> indexes = newTerms.replaceVariableName(oldName.toStdString(), newName.toStdString()); // In case of interaction, several terms might change
		if (indexes.size() > 0)
		{
			_setTerms(newTerms);
			for (int index : indexes)
			{
				QString newLabel = newTerms.at(index).asQString();
				SourceItem::SourceValuesItem elt = _labelValues.at(index);
				QString oldLabel = elt.label;
				elt.label = newLabel;
				_labelValues.replace(index, elt);
				_labelsMap.remove(oldLabel);
				_labelsMap[newLabel] = index;
				changedNamesMap[oldLabel] = newLabel;
			}
			changedIndexes += indexes;
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

