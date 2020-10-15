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

#include "boundqmlcomponentslist.h"
#include "analysis/jaspcontrolbase.h"
#include "rowcontrols.h"
#include "log.h"
#include "analysis/options/optionvariable.h"
#include "analysis/options/optionterm.h"

#include <QJsonValue>

BoundQMLComponentsList::BoundQMLComponentsList(JASPControlBase *item)
	: JASPControlWrapper(item)
	, QMLListView(item)
	, BoundQMLItem()
{
	_termsModel = new ListModelTermsAssigned(this);
	setTermsAreNotVariables();
	_termsModel->readModelProperty(this);

	QQuickItem::connect(_item, SIGNAL(nameChanged(int, QString)), this, SLOT(nameChangedHandler(int, QString)));
	QQuickItem::connect(_item, SIGNAL(valuesChanged()), this, SLOT(valuesChangedHandler()));
	QQuickItem::connect(_item, SIGNAL(addItem()), this, SLOT(addItemHandler()));
	QQuickItem::connect(_item, SIGNAL(removeItem(int)), this, SLOT(removeItemHandler(int)));
}

void BoundQMLComponentsList::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	if (!_boundTo)
	{
		Log::log()  << "Options for Components List " << name().toStdString() << " is not of type Table!" << std::endl;
		return;
	}

	Terms terms;
	QMap<QString, QMap<QString, Option*> > allOptionsMap;
	std::vector<Options*> optionsList = _boundTo->value();
	for (Options* options : optionsList)
	{
		std::string key;

		if (_termsModel->areTermsInteractions())
		{
			OptionTerm* termOption = dynamic_cast<OptionTerm*>(options->get(_optionKeyName));
			if (termOption)
			{
				Term term(termOption->term());
				key = term.asString();
				terms.add(term);
			}
			else
			{
				Log::log() << "Bind Option is not an OptionTerm in " << name().toStdString() << std::endl;
				return;
			}
		}
		else
		{
			OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKeyName));
			if (variableOption)
			{
				key = variableOption->variable();
				terms.add(Term(key));
			}
			else
			{
				Log::log() << "Bind Option is not an OptionVariable in " << name().toStdString() << std::endl;
				return;
			}
		}

		QMap<QString, Option*> optionsMap;
		for (const std::string& name : options->names)
			if (name != _optionKeyName)
				optionsMap[QString::fromStdString(name)] = options->get(name);

		allOptionsMap[QString::fromStdString(key)] = optionsMap;

	}

	_termsModel->initTerms(terms, allOptionsMap);
}

Option* BoundQMLComponentsList::createOption()
{
	Options* templote = new Options();

	if (_termsModel->areTermsInteractions())
		templote->add(_optionKeyName, new OptionTerm());
	else
		templote->add(_optionKeyName, new OptionVariable());
	addRowComponentsDefaultOptions(templote);

	OptionsTable* result = new OptionsTable(templote);
	std::vector<Options*> allOptions;

	if (hasSource())
	{
		Terms initTerms = _termsModel->getSourceTerms();
		for (const Term& term : initTerms)
		{
			Options* row = dynamic_cast<Options*>(templote->clone());
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm* optionVar = new OptionTerm();
				optionVar->setValue(term.scomponents());
				row->add(_optionKeyName, optionVar);
			}
			else
			{
				OptionVariables* optionVar = new OptionVariable();
				optionVar->setValue(term.asString());
				row->add(_optionKeyName, optionVar);
			}
			allOptions.push_back(row);
		}
	}
	else
	{
		QString			defaultName			= getItemProperty("newItemName").toString();
		QVariant		defaultValuesVar	= getItemProperty("defaultValues");
		QList<QVariant> defaultValues		= defaultValuesVar.toList();
		int				minimumItems		= getItemProperty("minimumItems").toInt();

		while (defaultValues.length() < minimumItems)
			defaultValues.push_back(defaultName);

		QList<QString> keyValues;
		QString keyName = tq(_optionKeyName);

		for (const QVariant& defaultValue : defaultValues)
		{
			QVariantMap defaultValueMap = defaultValue.toMap();
			Options* row = dynamic_cast<Options*>(templote->clone());
			OptionVariables* optionVar = new OptionVariable();

			QString keyValue = (defaultValue.type() == QVariant::String) ? defaultValue.toString() : defaultName;
			if (defaultValueMap.contains(keyName))
				keyValue = defaultValueMap[keyName].toString();

			keyValue = _makeUnique(keyValue, keyValues);
			keyValues.push_back(keyValue);

			optionVar->setValue(keyValue.toStdString());
			row->add(_optionKeyName, optionVar);

			QMapIterator<QString, QVariant> it(defaultValueMap);
			while (it.hasNext())
			{
				it.next();
				QString name = it.key();

				if (name != keyName)
				{
					QVariant value = it.value();
					Option* option = row->get(fq(name));
					if (option)		option->set(value.toJsonValue().toString().toStdString());
					else			Log::log() << "Default value with name " << name << " is unknown in the ComponentsList " << this->name() << std::endl;
				}
			}
			allOptions.push_back(row);
		}
	}
	result->connectOptions(allOptions);

	return result;
}

bool BoundQMLComponentsList::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool BoundQMLComponentsList::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::arrayValue;
}

void BoundQMLComponentsList::modelChangedHandler()
{
	if (_boundTo)
	{
		std::vector<Options*> allOptions;
		const Terms& terms = _termsModel->terms();
		const QMap<QString, RowControls*>& allControls = _termsModel->getRowControls();
		for (const Term& term : terms)
		{
			Options *rowOptions = static_cast<Options *>(_boundTo->rowTemplate()->clone());

			if (_termsModel->areTermsInteractions())
			{
				OptionTerm* optionTerm = dynamic_cast<OptionTerm*>(rowOptions->get(_optionKeyName));
				if (optionTerm)
					optionTerm->setValue(term.scomponents());
				else
					Log::log()  << "An option is not of type OptionTerm!!" << std::endl;
			}
			else
			{
				OptionVariable* optionVariable = dynamic_cast<OptionVariable*>(rowOptions->get(_optionKeyName));
				if (optionVariable)
					optionVariable->setValue(term.asString());
				else
					Log::log()  << "An option is not of type OptionVariable!!" << std::endl;
			}

			RowControls* rowControls = allControls[term.asQString()];
			if (rowControls)
			{
				const QMap<QString, JASPControlWrapper*>& controlsMap = rowControls->getJASPControlsMap();
				QMapIterator<QString, JASPControlWrapper*> it(controlsMap);
				while (it.hasNext())
				{
					it.next();
					BoundQMLItem* boundItem = dynamic_cast<BoundQMLItem*>(it.value());
					const QString& name = it.key();
					Option* option = boundItem->boundTo();
					rowOptions->add(name.toStdString(), option);
				}
			}
			allOptions.push_back(rowOptions);
		}

		_boundTo->connectOptions(allOptions);
	}
}

void BoundQMLComponentsList::valuesChangedHandler()
{
	_termsModel->readModelProperty(this);
	modelChangedHandler();
}

void BoundQMLComponentsList::addItemHandler()
{
	Terms newTerms;
	newTerms.add(_makeUnique(getItemProperty("newItemName").toString()));
	_termsModel->addTerms(newTerms);
	setItemProperty("currentIndex", _termsModel->rowCount() - 1);
}

void BoundQMLComponentsList::removeItemHandler(int index)
{
	_termsModel->removeTerm(index);
	setItemProperty("currentIndex", index >= _termsModel->rowCount() ? index - 1 : index);
}

void BoundQMLComponentsList::nameChangedHandler(int index, QString name)
{
	if (index < 0)
		return;
	if (index >= _termsModel->rowCount())
	{
		Log::log()  << "Index " << index << " in ListModelTabView is greater than the maximum " << _termsModel->rowCount() << std::endl;
		return;
	}

	if (name.isEmpty())
		name = getItemProperty("newItemName").toString();

	name = _makeUnique(name, index);

	_termsModel->changeTerm(index, name);
}

QString BoundQMLComponentsList::_changeLastNumber(const QString &val)
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

QString BoundQMLComponentsList::_makeUnique(const QString &val, int index)
{
	QList<QString> values = _termsModel->terms().asQList();

	return _makeUnique(val, values, index);
}

QString BoundQMLComponentsList::_makeUnique(const QString &val, const QList<QString> &values, int index)
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
