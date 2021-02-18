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

#include "componentslistbase.h"
#include "rowcontrols.h"
#include "log.h"

#include <QJsonValue>

ComponentsListBase::ComponentsListBase(QQuickItem *parent)
	: JASPListControl(parent), BoundControlBase(this)
{
	_controlType			= ControlType::ComponentsList;
	_useControlMouseArea	= false;
}

void ComponentsListBase::setUpModel()
{
	_termsModel = new ListModelTermsAssigned(this);
	JASPListControl::setUpModel();

	connect(this, &ComponentsListBase::nameChanged, this, &ComponentsListBase::nameChangedHandler);
	connect(this, &ComponentsListBase::addItem,		this, &ComponentsListBase::addItemHandler);
	connect(this, &ComponentsListBase::removeItem,	this, &ComponentsListBase::removeItemHandler);
}

void ComponentsListBase::bindTo(const Json::Value& value)
{
	BoundControlBase::bindTo(value);

	std::string keyName = fq(_optionKey);
	Terms terms;
	ListModel::RowControlsValues allControlValues;

	_readTableValue(value, keyName, containsInteractions(), terms, allControlValues);

	_termsModel->initTerms(terms, allControlValues);
}

Json::Value ComponentsListBase::createJson()
{
	std::string keyName = fq(_optionKey);

	Json::Value result(Json::arrayValue);

	if (hasSource())
	{
		Terms initTerms = _termsModel->getSourceTerms();
		for (const Term& term : initTerms)
		{
			Json::Value row(Json::objectValue);
			if (containsInteractions())
			{
				Json::Value keyTerm(Json::arrayValue);
				for (const std::string& term: term.scomponents())
					keyTerm.append(term);
				row[keyName] = keyTerm;
			}
			else
				row[keyName] = term.asString();
			result.append(row);
		}
	}
	else
	{
		QString			defaultName			= property("newItemName").toString();
		QVariant		defaultValuesVar	= property("defaultValues");
		QList<QVariant> defaultValues		= defaultValuesVar.toList();
		int				minimumItems		= property("minimumItems").toInt();

		while (defaultValues.length() < minimumItems)
			defaultValues.push_back(defaultName);

		QList<QString> keyValues;

		for (const QVariant& defaultValue : defaultValues)
		{
			QVariantMap defaultValueMap = defaultValue.toMap();
			Json::Value row(Json::objectValue);

			QString keyValue = (defaultValue.type() == QVariant::String) ? defaultValue.toString() : defaultName;
			if (defaultValueMap.contains(_optionKey))
				keyValue = defaultValueMap[_optionKey].toString();

			keyValue = _makeUnique(keyValue, keyValues);
			keyValues.push_back(keyValue);

			row[keyName] = fq(keyValue);

			QMapIterator<QString, QVariant> it(defaultValueMap);
			while (it.hasNext())
			{
				it.next();
				QString name = it.key();

				if (name != _optionKey)
				{
					QVariant valueVar = it.value();
					Json::Value valueJson;
					Json::Reader().parse(fq(valueVar.toString()), valueJson);
					row[fq(name)] = valueJson;
				}
			}
			result.append(row);
		}
	}

	return result;
}

bool ComponentsListBase::isJsonValid(const Json::Value &value)
{
	return value.isArray();
}

void ComponentsListBase::termsChangedHandler()
{
	const Terms& terms = _termsModel->terms();
	const QMap<QString, RowControls*>& allControls = _termsModel->getRowControls();

	_setTableValue(terms, allControls, fq(_optionKey), containsInteractions());
}

void ComponentsListBase::addItemHandler()
{
	Terms newTerms;
	newTerms.add(_makeUnique(property("newItemName").toString()));
	_termsModel->addTerms(newTerms);
	setProperty("currentIndex", _termsModel->rowCount() - 1);
}

void ComponentsListBase::removeItemHandler(int index)
{
	_termsModel->removeTerm(index);
	setProperty("currentIndex", index >= _termsModel->rowCount() ? index - 1 : index);
}

void ComponentsListBase::nameChangedHandler(int index, QString name)
{
	if (index < 0)
		return;
	if (index >= _termsModel->rowCount())
	{
		Log::log()  << "Index " << index << " in ListModelTabView is greater than the maximum " << _termsModel->rowCount() << std::endl;
		return;
	}

	if (name.isEmpty())
		name = property("newItemName").toString();

	name = _makeUnique(name, index);

	_termsModel->changeTerm(index, name);
}

QString ComponentsListBase::_changeLastNumber(const QString &val)
{
	QString result = val;
	int index = val.length() - 1;
	for (; index >= 0 ; index--)
	{
		if (!val.at(index).isDigit())
			break;
	}
	index++;

	int num = -1;
	if (index >= 0 && index < val.length())
	{
		bool ok = false;
		num = val.right(val.length() - index).toInt(&ok);
		if (!ok)
			num = -1;
	}

	if (num >= 0)
		return result.left(index).append(QString::number(num + 1));
	else
		return result.append(QString::number(2));
}

QString ComponentsListBase::_makeUnique(const QString &val, int index)
{
	QList<QString> values = _termsModel->terms().asQList();

	return _makeUnique(val, values, index);
}

QString ComponentsListBase::_makeUnique(const QString &val, const QList<QString> &values, int index)
{
	QString result = val;

	bool isUnique = true;
	do
	{
		int i = 0;
		isUnique = true;
		for (const QString& value : values)
		{
			if (i != index && value == result)
			{
				isUnique = false;
				result = _changeLastNumber(result);
			}
			i++;
		}
	} while (!isUnique);

	return result;
}
