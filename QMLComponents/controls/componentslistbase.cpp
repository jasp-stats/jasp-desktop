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
	connect(this, &ComponentsListBase::initializedChanged, this, &ComponentsListBase::resetDefaultValue);
	connect(this, &ComponentsListBase::headerLabelsChanged, this, &ComponentsListBase::controlNameXOffsetMapChanged);
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

void ComponentsListBase::setUp()
{
	JASPListControl::setUp();

	QQuickItem* ancestor = parentItem();
	while (ancestor)
	{
		JASPControl* control = qobject_cast<JASPControl*>(ancestor);
		if (control && control->dependsOnDynamicComponents())
		{
			control->addDependency(this);
			return;
		}

		ancestor = ancestor->parentItem();
	}
}

Json::Value ComponentsListBase::createJson() const
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
		QList<QVariant>	defaultValues		= property("defaultValues").toList();
		int				minimumItems		= property("minimumItems").toInt();

		while (defaultValues.length() < minimumItems)
			defaultValues.push_back(defaultName);

		QList<QString> keyValues;

		int nbOfRows = _defaultValues.length();
		if (_minimumItems > nbOfRows) nbOfRows = _minimumItems;
		for (int rowNb = 0; rowNb < nbOfRows; rowNb++)
		{
			QVariantMap defaultValuesMap;
			if (rowNb < _defaultValues.length()) defaultValuesMap = _defaultValues[rowNb].toMap();
			Json::Value row(Json::objectValue);

			QString keyValue = _newItemName;
			if (defaultValuesMap.contains(_optionKey))
				keyValue = defaultValuesMap[_optionKey].toString();

			keyValue = _makeUnique(keyValue, keyValues);
			keyValues.push_back(keyValue);

			row[keyName] = fq(keyValue);

			QMapIterator<QString, QVariant> it(defaultValuesMap);
			while (it.hasNext())
			{
				it.next();
				QString name = it.key();

				if (name != _optionKey)
				{
					QVariant valueVar = it.value();
					switch (valueVar.typeId())
					{
					case QMetaType::Int:		row[fq(name)] = valueVar.toInt();			break;
					case QMetaType::Double:		row[fq(name)] = valueVar.toDouble();		break;
					case QMetaType::Bool:		row[fq(name)] = valueVar.toBool();			break;
					case QMetaType::QString:	row[fq(name)] = fq(valueVar.toString());	break;
					default:
					{
						if (valueVar.canConvert<QString>())	row[fq(name)] = fq(valueVar.toString());
						else Log::log() << "Cannot convert default values with key " << name << " in ComponentList " << this->name() << std::endl;
					}
					}
				}
			}
			result.append(row);
		}
	}

	return result;
}

void ComponentsListBase::resetDefaultValue()
{
	if (hasSource()) return;

	// This slot is called when the component is initialized
	// If the default values are not given by the defaultValues property, then it must be set by the default values of
	// the components self. When the ComponentsList is initialized, the components are created and we can ask them their default values
	Json::Value		defaultJson			= createJson();
	uint			defaultValuesRows	= property("defaultValues").toList().size();

	if (defaultJson.size() > defaultValuesRows)
	{
		const Terms& terms = _termsModel->terms();
		const QMap<QString, RowControls*>& allRowControls = _termsModel->getAllRowControls();

		for (uint i = defaultValuesRows; i < defaultJson.size(); i++)
		{
			if (terms.size() > i)
			{
				const QString& key = terms.at(i).asQString();
				RowControls* rowControls = allRowControls[key];
				if (!rowControls)
				{
					Log::log() << "Cannot find " << key << " in row controls!" << std::endl;
					continue;
				}
				const QMap<QString, JASPControl*>& controlMap = rowControls->getJASPControlsMap();
				Json::Value row(Json::objectValue);

				row[fq(_optionKey)] = fq(key);
				for (const QString& controlName : controlMap.keys())
				{
					JASPControl* control = controlMap[controlName];
					BoundControl* boundControl = control->boundControl();
					if (boundControl)
						row[fq(controlName)] = boundControl->defaultBoundValue();
				}
				defaultJson[i] = row;
			}
		}
	}
	setDefaultBoundValue(defaultJson);
}



bool ComponentsListBase::isJsonValid(const Json::Value &value) const
{
	return value.isArray();
}

void ComponentsListBase::termsChangedHandler()
{
	JASPListControl::termsChangedHandler();

	_setTableValue(_termsModel->terms(), _termsModel->getTermsWithComponentValues(), fq(_optionKey), containsInteractions(), containsVariables());
	bindOffsets();
	emit controlNameXOffsetMapChanged();
}

void ComponentsListBase::bindOffsets()
{
	if (_termsModel && _termsModel->rowCount() > 0)
	{
		RowControls* row = _termsModel->getRowControls(_termsModel->terms().at(0).asQString());
		if (row)
			for (JASPControl* control : row->getJASPControlsMap().values())
			{
				connect(control, &JASPControl::xChanged, this, &ComponentsListBase::controlNameXOffsetMapChanged, Qt::UniqueConnection);
				connect(control, &JASPControl::visibleChanged, this, &ComponentsListBase::controlNameXOffsetMapChanged, Qt::UniqueConnection);
			}
	}
}

QList<QVariant> ComponentsListBase::controlNameXOffsetMap() const
{
	// The headers should be aligned with the controls of the ComponentsList
	// The headerLabels property is an array that can contain 2 kind of values:
	// . if it is just a string, then this is the header label of a ith control.
	// . if it is a map of a string to string, then this is the header label of a control name.
	// e.g.:
	// . headerLabels: ["label 1", "label 2", "label 3"]
	// . headerLabels: [{name2: "label 2", name1: "label 1"]
	// The mapping is used when not all the controls should get a header, or when a control can be invisible.
	// In theory, both kinds can be combined.
	// The first row of controls is used to determine the place (the x offset) of the headers.
	QList<QVariant> result;
	if (!_termsModel || _termsModel->rowCount() == 0)
		return result;

	std::vector<int> xOffsets;
	QMap<QString, int> nameXOffsetMap;
	QString key = _termsModel->terms().at(0).asQString();
	auto rowControls = _termsModel->getAllRowControls();
	if (!rowControls.contains(key))
		return result;

	RowControls* row = _termsModel->getRowControls(key);
	QList<JASPControl*> controls = row->getJASPControlsMap().values();

	// By going through the controls of the first row, 2 structures are set:
	// . xOffsets: gives which x offset is used by the ith control (sorted by x ascendent). This is used when the headerLabels are strings
	// . nameXOffsetMap: gives a mapping of control name/x offset. This is used when headerLabels are a name/label mapping.
	for (JASPControl* control : controls)
	{
		if (control->isVisible())
		{
			if (!nameXOffsetMap.contains(control->name()))
			{
				nameXOffsetMap[control->name()] = control->x();
				xOffsets.push_back(control->x());
			}
		}
	}
	if (xOffsets.size() > 0)
	{
		std::sort(xOffsets.begin(), xOffsets.end());
		for (const QVariant& labelVar : _headerLabels)
		{
			if (labelVar.typeId() == QMetaType::QString)
			{
				QString label = labelVar.toString();
				if (result.count() < xOffsets.size())
				{
					QMap<QString, QVariant> item {{"label", label}, {"x", xOffsets[result.count()]}};
					result.push_back(item);
				}
			}
			else if (labelVar.canConvert<QMap<QString, QVariant> >())
			{
				QMap<QString, QVariant> map = labelVar.toMap();
				for (const QString& controlName : map.keys())
				{
					if (nameXOffsetMap.contains(controlName))
					{
						QString label = map[controlName].toString();
						QMap<QString, QVariant> item {{"label", label}, {"x", nameXOffsetMap[controlName]}};
						result.push_back(item);
					}
					else
						Log::log() << "headerLabels property of ComponentsList " << name() << " makes reference of unknown control " << controlName << std::endl;
				}
			}
			else
				Log::log() << "headerLabels property of ComponentsList " << name() << " has wrong format." << std::endl;
		}
	}

	return result;
}

Json::Value ComponentsListBase::getJsonFromComponentValues(const Terms& terms, const ListModel::RowControlsValues &termsWithComponentValues)
{
	return _getTableValueOption(terms, termsWithComponentValues, fq(_optionKey), containsInteractions(), containsVariables());
}

void ComponentsListBase::addItemHandler()
{
	Terms newTerms;
	QString newTerm = _makeUnique(_newItemName);
	newTerms.add(newTerm);
	ListModel::RowControlsValues rowValues;

	if (_duplicateWhenAdding)
	{
		QMap<QString, Json::Value> jsonValues;
		
		const Json::Value	&	boundVal		= boundValue();
		int						currentIndex	= property("currentIndex").toInt();
		const Terms			&	terms			= _termsModel->terms();
		
		if (boundVal.isArray() && int(terms.size()) >= currentIndex)
		{
			std::string keyString = terms.at(size_t(currentIndex)).asString();
			
			for (const Json::Value& jsonVal : boundVal)
			{
				const Json::Value& keyVal = jsonVal.get(fq(_optionKey), Json::nullValue);
				
				if (keyVal.asString() == keyString)
				{
					for (const std::string& member : jsonVal.getMemberNames())
						jsonValues[tq(member)] = jsonVal.get(member, Json::nullValue);
				}
				
				jsonValues[_optionKey] = fq(newTerm);
			}
		}
		rowValues[newTerm] = jsonValues;
	}
	_termsModel->addTerms(newTerms, -1, rowValues);
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

	if (isBound())
	{
		// The name is the key value that allows to distinguish the elements of the components list.
		// So this must be also updated in the bound value.
		Json::Value val = boundValue();
		if (val.isArray() && val.size() > index)
		{
			val[index][fq(_optionKey)] = fq(name);
			setBoundValue(val, false);
		}
	}

	_termsModel->changeTerm(index, name);
}

QString ComponentsListBase::_changeLastNumber(const QString &val) const
{
	QString result = val;
	
	int index;
	for (index = val.length() - 1; index >= 0 ; index--)
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

QString ComponentsListBase::_makeUnique(const QString &val, int index) const
{
	QList<QString> values = _termsModel->terms().asQList();

	return _makeUnique(val, values, index);
}

QString ComponentsListBase::_makeUnique(const QString &val, const QList<QString> &values, int index) const
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
