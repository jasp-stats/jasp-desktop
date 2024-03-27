#include "column.h"
#include "dataset.h"
#include "log.h"
#include "columnutils.h"
#include "databaseinterface.h"

Column::Column(DataSet * data, int id)
	: DataSetBaseNode(dataSetBaseNodeType::column, data->dataNode()),
	_data(data),
	_id(id),
	_emptyValues(new EmptyValues(data->emptyValues())),
	_doubleDummy(new Label(this))
{}

Column::~Column()
{
	labelsTempReset();
	delete _emptyValues;
	delete _doubleDummy;
}

void Column::dbCreate(int index)
{
	JASPTIMER_SCOPE(Column::dbCreate);

	assert(_id == -1);
	db().columnInsert(_id, index);
}

void Column::dbLoad(int id, bool getValues)
{
	JASPTIMER_SCOPE(Column::dbLoad);

	assert(_id == id || (id != -1 && _id == -1) || _id != -1);

	if(id != -1)
		_id = id;

	db().transactionReadBegin();
	
	Json::Value emptyVals;
	
	db().columnGetBasicInfo(	_id, _name, _title, _description, _type, _revision, emptyVals);
	db().columnGetComputedInfo(	_id, _analysisId, _invalidated, _codeType, _rCode, _error, _constructorJson);
	
	_emptyValues->fromJson(emptyVals);

	labelsTempReset();
	db().labelsLoad(this);
	
	if(getValues)
		db().columnGetValues(_id, _ints, _dbls);


	db().transactionReadEnd();
}

void Column::dbLoadIndex(int index, bool getValues)
{
	JASPTIMER_SCOPE(Column::dbLoadIndex);

	_id = db().columnIdForIndex(_data->id(), index);

	dbLoad(_id, getValues);
}

void Column::dbDelete(bool cleanUpRest)
{
	assert(_id != -1);

	labelsClear();
	db().columnDelete(_id, cleanUpRest);

	_id = -1;
}

void Column::loadComputedColumnJsonBackwardsCompatibly(const Json::Value & json)
{
	//Columnname is used to find this:
	//_column = DataSetPackage::pkg()->dataSet()->column(json["name"].asString());
	
	const std::string & rCode = json["rCode"].asString();

	setCompColStuff
	(
		json["invalidated"].asBool(),
		computedColumnTypeFromString(json["codeType"].asString()),
		rCode,
		json["error"].asString(),
		json["constructorCode"]
	);
}

void Column::invalidateDependents()
{
	for(Column * col : _data->columns())
		if(col->isComputed() && col->dependsOn(name()))
			col->invalidate();
}

void Column::setName(const std::string &name)
{
	JASPTIMER_SCOPE(Column::setName);

	if(_name == name)
		return;

	std::string orgName = _name;
	_name = getUniqueName(name);

	if(_title.empty() || _title == orgName)
		setTitle(_name);

	db().columnSetName(_id, _name);
	incRevision();
}

void Column::setTitle(const std::string &title)
{
	JASPTIMER_SCOPE(Column::setTitle);

	if(_title == title)
		return;

	_title = title;
	db().columnSetTitle(_id, _title);
	incRevision();
}

void Column::setDescription(const std::string &description)
{
	JASPTIMER_SCOPE(Column::setDescription);

	if(_description == description)
		return;

	_description = description;
	db().columnSetDescription(_id, _description);
	incRevision();
}

void Column::setType(columnType colType)
{
	JASPTIMER_SCOPE(Column::setType);

	if(_type == colType)
		return;
	
	_type = colType;
	db().columnSetType(_id, _type);
	incRevision();
}

bool Column::hasCustomEmptyValues() const
{
	return _emptyValues->hasEmptyValues();
}

void Column::setHasCustomEmptyValues(bool hasCustom)
{
	JASPTIMER_SCOPE(Column::setHasCustomEmptyValues);

	if(hasCustomEmptyValues() == hasCustom)
		return;
	
	_emptyValues->setHasCustomEmptyValues(hasCustom);
	db().columnSetEmptyVals(_id, _emptyValues->toJson().toStyledString());
	
	incRevision();
}

bool Column::setCustomEmptyValues(const stringset& customEmptyValues)
{
	JASPTIMER_SCOPE(Column::setCustomEmptyValues);
	
	if (_emptyValues->emptyStrings() == customEmptyValues)
		return false;

	_emptyValues->setEmptyValues(customEmptyValues, _emptyValues->hasEmptyValues());
	db().columnSetEmptyVals(_id, _emptyValues->toJson().toStyledString());
	
	incRevision();
	
	return true;
}

void Column::dbUpdateComputedColumnStuff()
{
	db().columnSetComputedInfo(_id, _analysisId, _invalidated, _codeType, _rCode, _error, constructorJsonStr());
	incRevision();
}

void Column::setInvalidated(bool invalidated)
{
	JASPTIMER_SCOPE(Column::setInvalidated);

	if(_invalidated == invalidated)
		return;
	
	_invalidated = invalidated;
	db().columnSetInvalidated(_id, _invalidated);
	incRevision(false);
}

void Column::setCodeType(computedColumnType codeType)
{
	JASPTIMER_SCOPE(Column::setCodeType);

	if(codeType == _codeType)
		return;

	if(_codeType == computedColumnType::notComputed)
	{
		_constructorJson = Json::objectValue;
		_constructorJson["formulas"] = Json::arrayValue;
	}

	if(_codeType == computedColumnType::analysisNotComputed)
		_analysisId = -1;

	_codeType	= codeType;
	
	dbUpdateComputedColumnStuff();
}

bool Column::setConstructorJson(const std::string & constructorJson)
{
	Json::Value parsed;
	Json::Reader().parse(constructorJson, parsed);
	
	return setConstructorJson(parsed);
}

bool Column::setConstructorJson(const Json::Value &constructorJson)
{
	JASPTIMER_SCOPE(Column::setConstructorJson);
	
	//Log::log() << "setConstructorJson got " << constructorJson.toStyledString() << " and had: " << _constructorJson.toStyledString() << std::endl;

	if(_constructorJson == constructorJson)
		return false;
	
	_constructorJson = constructorJson;

	dbUpdateComputedColumnStuff();
	
	return true;
}

bool Column::setRCode(const std::string & rCode)
{
	JASPTIMER_SCOPE(Column::setRCodes);

	if(_rCode == rCode)
		return false;
	
	_rCode			= rCode;
	
	findDependencies();
	invalidate();
	
	dbUpdateComputedColumnStuff();
	
	return true;
}

bool Column::setError(const std::string & error)
{
	JASPTIMER_SCOPE(Column::setError);

	if(error == _error)
		return false;
	
	_error = error;
	
	dbUpdateComputedColumnStuff();
	
	return true;
}

void Column::setAnalysisId(int analysisId)
{
	JASPTIMER_SCOPE(Column::setAnalysisId);

	if(_analysisId == analysisId)
		return;
	
	_analysisId = analysisId;
	
	dbUpdateComputedColumnStuff();
}

bool Column::iShouldBeSentAgain()
{
	if(!invalidated()) 
		return false;

	for(Column * col : _data->columns())
		if(col->isComputed() && dependsOn(col->name()) && col->invalidated())
			return false;
	return true;
}


void Column::setCompColStuff(bool invalidated, computedColumnType codeType, const std::string &rCode, const std::string &error, const Json::Value &constructorJson)
{
	JASPTIMER_SCOPE(Column::setCompColStuff);

	_invalidated		= invalidated;
	_codeType			= codeType;
	_rCode				= rCode;
	_constructorJson	= constructorJson;
	
	dbUpdateComputedColumnStuff();
}

intset Column::getUniqueLabelValues() const
{
	JASPTIMER_SCOPE(Column::getUniqueLabelValues);
	intset vals;

	for(const Label * label : _labels)
		vals.insert(label->intsId());
	
	return vals;
}

void Column::_convertVectorIntToDouble(intvec & intValues, doublevec & doubleValues)
{
	JASPTIMER_SCOPE(Column::_convertVectorIntToDouble);

	doubleValues.clear();
	for (int intValue : intValues)
		doubleValues.push_back(intValue == EmptyValues::missingValueInteger ? EmptyValues::missingValueDouble : double(intValue));
}

columnTypeChangeResult Column::changeType(columnType colType)
{
	JASPTIMER_SCOPE(Column::changeType);
	
	if(!isComputed())
	{
		setType(colType);
		return columnTypeChangeResult::changed;
		
	}
	else
	{
		if(codeType() == computedColumnType::analysis)
			return columnTypeChangeResult::generatedFromAnalysis;

		setDefaultValues(colType);
		invalidate();
		return columnTypeChangeResult::changed;	
	}
}

void Column::setDefaultValues(enum columnType columnType)
{
	JASPTIMER_SCOPE(Column::setDefaultValues);

	setType(columnType);
	
	for(size_t i=0; i<_ints.size(); i++)
	{
		_ints[i] =  EmptyValues::missingValueInteger;
		_dbls[i] =  EmptyValues::missingValueDouble;
	}
	
	labelsClear();
	
	dbUpdateValues(false);
}

void Column::dbUpdateValues(bool labelsTempCanBeMaintained)
{
	if(!_data->writeBatchedToDB())
		db().columnSetValues(_id, _ints, _dbls);
	
	incRevision(labelsTempCanBeMaintained);
}

columnType Column::resetValues(int thresholdScale)
{
	return setValues(valuesAsStrings(), labelsAsStrings(), thresholdScale);
}

stringset Column::mergeOldMissingDataMap(const Json::Value &missingData)
{
	stringset foundEmpty;
	
	std::map<std::string, Label*> displayToLabel; //Keep track of which labels we added because only those could possibly be derived from missingDataMap
	for(qsizetype r=0;	r<_ints.size(); r++)
	{
		const std::string row = std::to_string(r);
		if(missingData.isMember(row))
		{
					double			dbl				= EmptyValues::missingValueDouble;
			const	std::string &	displayValue	= missingData[row].asString();
									foundEmpty		. insert(displayValue);
					bool			isDbl			= ColumnUtils::getDoubleValue(	displayValue,	dbl);
					Label		*	label			= displayToLabel.count(displayValue)										
													? displayToLabel.at(displayValue)											// Get previously created label, or:
													: displayToLabel[displayValue]	= isDbl										// Be careful, we add something to the map and use the returnvalue from the assignment to also get it into `label`
																					? nullptr									// Numbers dont need a label
																					: labelByIntsId(labelsAdd(displayValue));	// And here we do, because where else are we going to store that string?
									_ints[r]		= label ? label->intsId() : Label::DOUBLE_LABEL_VALUE;
									_dbls[r]		= dbl;
		}
	}
	
	foundEmpty.erase(""); //So for some currently inscrutable reason empty strings were also stored in the missing data map... Remove any occurences.
	
	return foundEmpty;
}

columnType Column::setValues(const stringvec & values, int thresholdScale, bool * aChange)
{
	return setValues(values, values, thresholdScale, aChange);
}

columnType Column::setValues(const stringvec & values, const stringvec & labels, int thresholdScale, bool * aChange)
{
	JASPTIMER_SCOPE(Column::setValues);

	if(aChange && _dbls.size() != values.size())
		(*aChange) = true;

	assert(values.size() == labels.size());

	_dbls.resize(values.size());
	_ints.resize(values.size());
	
	bool	onlyDoubles = true, 
			onlyInts	= true;
	
	intset ints;  // to suggest whether this is a scalar or not we need to know whether we have more than treshold ints or not.
	
	for(size_t i=0; i<values.size(); i++)
	{
		const std::string	&	value	= values[i],
							&	labelT	= labels[i];
		double					dblVal	= EmptyValues::missingValueDouble;
		int						intVal	= EmptyValues::missingValueInteger;
		
		//all integers are also doubles, so we need not make Labels for them immediately.
		//At least, if the labelT is the same as the value!
		if((value.empty() || ColumnUtils::getDoubleValue(value, dblVal)) && value == labelT)
		{
			if(aChange && !Utils::isEqual(_dbls[i], dblVal))
				(*aChange) = true;
			
			_ints[i] = Label::DOUBLE_LABEL_VALUE;	//Just use whatever is in the dbl
			_dbls[i] = dblVal;
			
			if(ColumnUtils::getIntValue(value, intVal))			ints.insert(intVal);
			else if(!value.empty())								onlyInts = false;
		}
		else //Not integer or double, or has a different label than value, so make or find a Label
		{
			Json::Value originalValue = value;

			if(ColumnUtils::getIntValue(value, intVal))
			{
				originalValue = intVal;
				ints.insert(intVal);
			}

			else if(ColumnUtils::getDoubleValue(value, dblVal))
				originalValue = dblVal;


			Label * label		= labelByValue(value);
					label		= label ? label : labelByDisplay(labelT); // If we didnt find it by value maybe try by display? Not sure if this is a good idea though
					_ints[i]	= label ? label->intsId() : labelsAdd(labelT, "", originalValue); // if we still didnt find a label we create it, and we write the intsId of whatever label we now have to _ints
					_dbls[i]	= originalValue.isInt() ? originalValue.asInt() : originalValue.isDouble() ? originalValue.asDouble() : EmptyValues::missingValueDouble;
			Label * newLabel	= labelByIntsId(_ints[i]);
			
			if(aChange && label != newLabel)
				(*aChange) = true;
			
			//	We should register that we found something not double or int, unless there is no label.
			//	Because then it is a bona fida missing value (as in actually missing)
			//	Otherwise check with the label whether it is an empty value, to ignore when determining the best columnType
			if(newLabel && !newLabel->isEmptyValue()) 
			{
				if(!originalValue.isDouble())
					onlyDoubles = false;

				if(!originalValue.isInt())
					onlyInts	= false;
			}
		}
	}
	
	dbUpdateValues(false);
	
	//Now determine what the most logical columntype would be given the current values AND empty values!
	if(onlyInts && ints.size() <= thresholdScale && ints.size() > 0)
	{
		if(ints.size() == 2)				return columnType::nominal;
		if(ints.size() <= thresholdScale)	return columnType::ordinal;
		return columnType::scale;
	}
	
	if(onlyDoubles)
		return columnType::scale;
	
	
	//So not everything was an int or a dbl so lets go through the current ints, count total labels and determine whether it might be nominal or ordinal
	std::set<double> doublesNonNA;
	std::set<Label*> labelsNonNA;
	
	for(size_t i=0; i<_ints.size(); i++)
	{
		int value = _ints[i];
		
		if(value != Label::DOUBLE_LABEL_VALUE)
		{
			Label * label = labelByIntsId(value);
			
			if(label && !label->isEmptyValue())
				labelsNonNA.insert(label);
		}
		else if(!std::isnan(_dbls[i]))
			doublesNonNA.insert(_dbls[i]);
	}
	
	size_t howManyLabelLike = doublesNonNA.size() && labelsNonNA.size();

	if(howManyLabelLike <= 2 || howManyLabelLike > thresholdScale)
		return columnType::nominal;
	
	
	return columnType::ordinal;
}

bool Column::setDescriptions(strstrmap labelToDescriptionMap)
{
	JASPTIMER_SCOPE(Column::setDescriptions);
	
	bool anyChange = false;
	
	for(const auto & labelDesc : labelToDescriptionMap)
	{
		Label * label = labelByDisplay(labelDesc.first);
		
		if(label)
		{
			anyChange = label->setDescription(labelDesc.second) || anyChange;
			continue;
		}
		
		if(labelDesc.first == labelDesc.second) //If we have no label but the map doesnt make any changes then ignore it
			continue;
		
		//Maybe it looks like a double? Then we should probably change add a double?
		double doubleValue = EmptyValues::missingValueDouble;
		if(ColumnUtils::getDoubleValue(labelDesc.first, doubleValue) && !std::isnan(doubleValue))
		{
			//Lets create a label
			int labelValue = labelsAdd(labelDesc.first, labelDesc.second, doubleValue);
			
			anyChange = true;
			
			//Now we need to make sure that the _ints category points to the label
			for(size_t row=0; row<_ints.size(); row++)
				if(Utils::isEqual(_dbls[row], doubleValue))
				{
					if(_ints[row] != Label::DOUBLE_LABEL_VALUE && _ints[row] != labelValue)
						Log::log() << "Column(" << name() << ")::setDescriptions(...)\n" << "_ints[" << row << "] != Label::DOUBLE_LABEL_VALUE && _ints[" << row << "] != labelValue (" << labelValue << ")" << std::endl;
					_ints[row] = labelValue;
				}
		}
	}
	
	return anyChange;
}


bool Column::overwriteDataAndType(stringvec data, columnType colType)
{
	JASPTIMER_SCOPE(Column::overwriteDataAndType);

	if(data.size() != _data->rowCount())
		data.resize(_data->rowCount());

	bool changes = _type != colType;
	setValues(data,0, & changes);
	setType(colType);
	
	return changes;
}

void Column::_dbUpdateLabelOrder(bool noIncRevisionWhenBatchedPlease)
{
	JASPTIMER_SCOPE(Column::_dbUpdateLabelOrder);
	
	if(batchedLabel())
	{
		if(!noIncRevisionWhenBatchedPlease)
			incRevision();
		return;
	}

	intintmap orderPerDbIds;

	for(size_t i=0; i<_labels.size(); i++)
	{
		_labels[i]->setOrder(i);
		orderPerDbIds[_labels[i]->dbId()] = i;
	}

	db().labelsSetOrder(orderPerDbIds);
	
	incRevision();
}

void Column::_sortLabelsByOrder()
{
	std::sort(_labels.begin(), _labels.end(), [](const Label * l, const Label * r) { return l->order() < r->order(); });
}

void Column::labelsClear()
{
	db().labelsClear(_id);
	_labels.clear();
	_labelByIntsIdMap.clear();
	
	labelsTempReset();

	incRevision();
}

void Column::beginBatchedLabelsDB()
{
	assert(!_batchedLabel);
	_batchedLabel = true;
}

void Column::endBatchedLabelsDB(bool wasWritingBatch)
{
	assert(_batchedLabel);
	_batchedLabel = false;
	
	for(size_t i=0; i<_labels.size(); i++)
		_labels[i]->setOrder(i);

	if(wasWritingBatch)
	{
		db().labelsWrite(this);
		incRevision(); //Should trigger reload at engine end
	}
	else
		_sortLabelsByOrder();
}

int Column::labelsAdd(int display)
{
	return labelsAdd(std::to_string(display));
}

int Column::labelsAdd(const std::string &display)
{
	if(display == "")
		return EmptyValues::missingValueInteger;
	
	int		anInt;
	double	aDouble;
	
	Json::Value original = display;
	
	if		(ColumnUtils::getIntValue(		display, anInt))	original = anInt;
	else if	(ColumnUtils::getDoubleValue(	display, aDouble))	original = aDouble;

	return labelsAdd(display, "", original);
}

int Column::labelsAdd(const std::string & display, const std::string & description, const Json::Value & originalValue)
{
	return labelsAdd(_labels.size(), display, true, description, originalValue);
}

int Column::labelsAdd(int value, const std::string & display, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order, int id)
{
	JASPTIMER_SCOPE(Column::labelsAdd);

	Label * label = new Label(this, display, value, filterAllows, description, originalValue, order, id);
	_labels.push_back(label);
	
	_labelByIntsIdMap[label->intsId()] = label;

	_dbUpdateLabelOrder(true);
	return label->intsId();
}

void Column::labelsRemoveValues(std::set<int> valuesToRemove)
{
	if (valuesToRemove.empty()) return;

	JASPTIMER_SCOPE(Column::labelsRemoveValues);

	_labels.erase(
		std::remove_if(
			_labels.begin(),
			_labels.end(),
			[&](Label * label) {
				if(std::find(valuesToRemove.begin(), valuesToRemove.end(), label->intsId()) != valuesToRemove.end())
				{
						_labelByIntsIdMap.erase(label->intsId());
					label->dbDelete();
					delete label;
					return true;
				}
				return false;
			}),
			_labels.end());

	_dbUpdateLabelOrder();
}

strintmap Column::labelsResetValues(int & maxValue)
{
	JASPTIMER_SCOPE(Column::labelsResetValues);
	
	bool wasBatching = batchedLabel();
	
	if(!wasBatching)
		beginBatchedLabelsDB();

	strintmap result;
	int labelValue = 0;
	_labelByIntsIdMap.clear();

	for (Label * label : _labels)
	{
		if (label->intsId() != labelValue)
			label->setIntsId(labelValue);

		result[label->label()] = labelValue;

		_labelByIntsIdMap[labelValue] = label;

		labelValue++;
	}

	maxValue = labelValue;

	if(!wasBatching)
		endBatchedLabelsDB();

	return result;
}

void Column::labelsRemoveBeyond(size_t indexToStartRemoving)
{
	for(size_t i=indexToStartRemoving; i<_labels.size(); i++)
		delete _labels[i];
	
	_labels.resize(indexToStartRemoving);
	
	_resetLabelValueMap();
}

void Column::labelsTempReset()
{
	_labelsTemp			. clear();
	_labelsTempDbls		. clear();
	_labelsTempToIndex	. clear();
	_labelsTempRevision = -1;
	_labelsTempMaxWidth = 0;
}

int Column::labelsTempCount()
{
	if(_revision != _labelsTempRevision)
	{
		//first collect the labels that are actually Label
		_labelsTemp			. clear();
		_labelsTempDbls		. clear();
		_labelsTemp			. reserve(_labels.size());
		_labelsTempDbls		. reserve(_labels.size());
		_labelsTempToIndex	. clear();
		_labelsTempMaxWidth = 0;
		
		for(size_t r=0; r<_labels.size(); r++)
			if(!_labels[r]->isEmptyValue())
			{
				_labelsTemp.push_back(		_labels[r]->label());
				_labelsTempDbls.push_back(	_labels[r]->originalValue().isDouble() ? _labels[r]->originalValue().asDouble() : EmptyValues::missingValueDouble);
				_labelsTempToIndex[_labelsTemp[_labelsTemp.size()-1]]	= _labelsTemp.size()-1; //We store the index in _labelsTemp in a map.
			}
		
		//There might also be "double" values that should also be shown in the editor so we go through everything and add them to 	_labelsTemp and _labelsTempToIndex	
		for(size_t r=0; r<rowCount(); r++)
			if(_ints[r] == Label::DOUBLE_LABEL_VALUE)
			{
				const std::string doubleLabel = doubleToDisplayString(_dbls[r], false);
				
				if(!doubleLabel.empty() && !_labelsTempToIndex.count(doubleLabel))
				{
					_labelsTemp						. push_back(doubleLabel);
					_labelsTempDbls					. push_back(_dbls[r]);
					_labelsTempToIndex[doubleLabel] = _labelsTemp.size()-1;
					_labelsTempMaxWidth				= std::max(_labelsTempMaxWidth, qsizetype(_labelsTemp[_labelsTemp.size()-1].size()));
				}
			}

		//Make sure we dont do this too often be remembering at which revision we created the temp values:
		_labelsTempRevision = _revision;
	}
	
	return _labelsTemp.size();
}

const stringvec &Column::labelsTemp()
{
	labelsTempCount(); //generate the list if need be
	
	return _labelsTemp;
}

std::string Column::labelsTempDisplay(size_t tempLabelIndex)
{
	if(labelsTempCount() <= tempLabelIndex)
		return "";
	
	return _labelsTemp[tempLabelIndex];
}

std::string Column::labelsTempValue(size_t tempLabelIndex, bool fancyEmptyValue)
{
	if(labelsTempCount() <= tempLabelIndex)
		return "";
	
	if(tempLabelIndex < _labels.size())
		return _labels[tempLabelIndex]->originalValueAsString(fancyEmptyValue);
	
	//So its not from a Label, this means its from _dbls
	//So that means the display value is actually the same as the value so:
	return doubleToDisplayString(_labelsTempDbls[tempLabelIndex], fancyEmptyValue);
}

double Column::labelsTempValueDouble(size_t tempLabelIndex)
{
	if(labelsTempCount() <= tempLabelIndex)
		return EmptyValues::missingValueDouble;
	
	if(tempLabelIndex < _labels.size())
		return _labels[tempLabelIndex]->originalValue().isDouble() ? _labels[tempLabelIndex]->originalValue().asDouble() : EmptyValues::missingValueDouble;
	
	//So its not from a Label, this means its from _dbls
	return _labelsTempDbls[tempLabelIndex];
}

int Column::labelsDoubleValueIsTempLabelRow(double dbl)
{
	for(size_t r=0; r<labelsTempCount(); r++)
		if(Utils::isEqual(dbl, _labelsTempDbls[r]))
			return r;
	return -1;
}

void Column::_resetLabelValueMap()
{
	_labelByIntsIdMap.clear();
	for(Label * label : _labels)
		_labelByIntsIdMap[label->intsId()] = label;
	
	labelsTempReset();
}

std::string Column::_getLabelDisplayStringByValue(int key, bool ignoreEmptyValue) const
{
	if (key == EmptyValues::missingValueInteger)
		return EmptyValues::displayString();
	
	if(_labelByIntsIdMap.count(key))
	{
		return	ignoreEmptyValue 
			?	_labelByIntsIdMap.at(key)->labelIgnoreEmpty()
			:	_labelByIntsIdMap.at(key)->labelDisplay();
	}

	return std::to_string(key);
}

std::string Column::getValue(size_t row, bool fancyEmptyValue) const
{
	if (row < rowCount())
	{
		if (_type == columnType::scale || _ints[row] == Label::DOUBLE_LABEL_VALUE)
			return doubleToDisplayString(_dbls[row], fancyEmptyValue);

		else if (_ints[row] != EmptyValues::missingValueInteger)
		{
			Label * label = labelByIntsId(_ints[row]);

			if(label)
				return label->originalValueAsString(fancyEmptyValue);
		}
	}
	
	return fancyEmptyValue ? EmptyValues::displayString() : "";
}

std::string Column::getDisplay(size_t row, bool fancyEmptyValue) const
{
	if (row < rowCount())
	{
		if (_type == columnType::scale || _ints[row] == Label::DOUBLE_LABEL_VALUE)
			return doubleToDisplayString(_dbls[row], fancyEmptyValue);
		else
			return _getLabelDisplayStringByValue(_ints[row]);
	}
	
	return fancyEmptyValue ? EmptyValues::displayString() : "";
}

std::string Column::getLabel(size_t row, bool fancyEmptyValue, bool ignoreEmptyValue) const
{
	if (row < rowCount())
	{
		if (_ints[row] == Label::DOUBLE_LABEL_VALUE)
			return doubleToDisplayString(_dbls[row], fancyEmptyValue, ignoreEmptyValue);
		else
			return _getLabelDisplayStringByValue(_ints[row], ignoreEmptyValue);
	}
	
	return fancyEmptyValue ? EmptyValues::displayString() : "";
}

std::string Column::doubleToDisplayString(double dbl, bool fancyEmptyValue, bool ignoreEmptyValue) const
{
	ignoreEmptyValue = ignoreEmptyValue && !std::isnan(dbl);
	
	if (isEmptyValue(dbl) && !ignoreEmptyValue)				return fancyEmptyValue ? EmptyValues::displayString() : "";
	else													return ColumnUtils::doubleToString(dbl);
}

std::string Column::operator[](size_t row)
{
	return getDisplay(row);
}

stringvec Column::valuesAsStrings() const
{
	stringvec returnMe;
	returnMe.resize(_dbls.size());
	
	for(size_t i=0; i<returnMe.size(); i++)
		returnMe[i] = getValue(i);
	
	return returnMe;
}

stringvec Column::labelsAsStrings() const
{
	stringvec returnMe;
	returnMe.resize(_dbls.size());
	
	for(size_t i=0; i<returnMe.size(); i++)
		returnMe[i] = getLabel(i);
	
	return returnMe;
}

stringvec Column::displaysAsStrings() const
{
	stringvec returnMe;
	returnMe.resize(_dbls.size());
	
	for(size_t i=0; i<returnMe.size(); i++)
		returnMe[i] = getDisplay(i);
	
	return returnMe;
}

stringvec Column::dataAsRLevels(intvec & values, const boolvec & filter, bool useLabels ) const
{
	stringvec	levels;
	stringset	levelsIncluded,
				levelsAdded;
	
	auto _addLabel = [&](const std::string & display, bool fromData)
	{
		if(!levelsAdded.count(display))
		{
			levelsAdded.insert(display);
			levels.push_back(display);
		}
		
		if(fromData)
			levelsIncluded.insert(display);
	};
	
	//First we try to find all levels, start with the known labels and then add any  doubles as labels.
	for(Label * label : _labels)
		if(!label->isEmptyValue())
			_addLabel(useLabels ? label->labelDisplay() : label->originalValueAsString(false), false);
	
	assert(filter.size() == rowCount() || filter.size() == 0);

	//We ignore emptyvalues and depending on whether filter is usable (length is data length) we filter out rows we dont need
	bool useFilter = filter.size() == rowCount();
	
	for(size_t row=0; row<rowCount(); row++)
		if(!useFilter || filter[row])
		{
			if(_ints[row] != Label::DOUBLE_LABEL_VALUE)
			{
				Label * label = labelByIntsId(_ints[row]);
				
				assert(label);
				
				if(!label->isEmptyValue())
					_addLabel(useLabels ? label->labelDisplay() : label->originalValueAsString(false), true);
			}
			else
			{
				double val = _dbls[row];
				
				if(!isEmptyValue(val))
					_addLabel(doubleToDisplayString(val, false), true);
			}
		}
	
	//At the end we make a mapping of the levels we have and need
	//We make sure the map is up to date afterwards
	for(int levelI=levels.size()-1; levelI >= 0; levelI--)
		if(!levelsIncluded.count(levels[levelI]))
			levels.erase(levels.begin() + levelI);
	
	strintmap levelToValueMap;
	for(size_t levelI=0; levelI<levels.size(); levelI++)
		levelToValueMap[levels[levelI]] = levelI;
	
	//Then we fill values with the correct values
	values.resize(0); //make sure there is nothing in it
	
	for(size_t row=0; row<rowCount(); row++)
		if(!useFilter || filter[row])
		{
			if(_ints[row] != Label::DOUBLE_LABEL_VALUE)
			{
				Label * label = labelByIntsId(_ints[row]);
				
				assert(label);
				
				if(!label->isEmptyValue())
					values.push_back(levelToValueMap[useLabels ? label->labelDisplay() : label->originalValueAsString(false)]);
				else
					values.push_back(EmptyValues::missingValueInteger);
			}
			else
			{
				double val = _dbls[row];
				
				if(!isEmptyValue(val))
					values.push_back(levelToValueMap[doubleToDisplayString(val, false)]);
				else
					values.push_back(EmptyValues::missingValueInteger);
			}
		}
	
	return levels;
}

doublevec Column::dataAsRDoubles(const boolvec &filter) const
{
	doublevec doubles;
		
	assert(filter.size() == rowCount() || filter.size() == 0);

	//depending on whether filter is usable (length is data length) we filter out rows we dont need
	bool useFilter = filter.size() == rowCount();
	
	for(size_t row=0; row<rowCount(); row++)
		if(!useFilter || filter[row])
			doubles.push_back(!isEmptyValue(_dbls[row]) ? _dbls[row] : EmptyValues::missingValueDouble);
				
	return doubles;
}

Label *Column::replaceDoubleWithLabel(double dbl)
{
	return replaceDoubleWithLabel(doublevec(dbl))[dbl];
}

std::map<double, Label*> Column::replaceDoubleWithLabel(doublevec dbls)
{
	JASPTIMER_SCOPE(Column::replaceDoubleWithLabel);
	
	std::map<double, Label*	>	doubleLabelMap;
	std::map<double, int	>	doubleIntIdMap;
	
	beginBatchedLabelsDB();
	for(double dbl : dbls)
		if(!doubleIntIdMap.count(dbl))
		{
			assert(!std::isnan(dbl)); //Because why would we be replacing it then?
			doubleIntIdMap[dbl] = labelsAdd(doubleToDisplayString(dbl, false), "", dbl);
			doubleLabelMap[dbl] = labelByIntsId(doubleIntIdMap[dbl]);
		}
	endBatchedLabelsDB();
	
	for(size_t r=0; r<rowCount(); r++)
		if(!std::isnan(_dbls[r]) && doubleIntIdMap.count(_dbls[r]))
			_ints[r] = doubleIntIdMap[_dbls[r]]; 
	
	dbUpdateValues();
	
	return doubleLabelMap;
}

Label *Column::replaceDoublesTillLabelsRowWithLabels(size_t row)
{
	//row here is in the label editor, not in the data!	
	if(row < _labels.size())
		return _labels[row];
	
	bool		labelsTempUpToDate = _revision == _labelsTempRevision; //If so we can just update it at the end. We dont not yet need to recreate them
	doublevec	dbls;
	double		dbl;
	
	for(size_t r=_labels.size()			;
		r<=row && r<_labelsTemp.size()	;
		r++						
	)
		if(ColumnUtils::getDoubleValue(_labelsTemp[r], dbl))
			dbls.push_back(dbl);
		else
			throw std::runtime_error("replaceDoublesTillLabelsRowWithLabels choked on a temp-label that cant be converted to double???"); //Should never ever occur because it starts from _labels.size!

	//the last dbl is the one we want so use it to get the right label from the map:
	Label * label = replaceDoubleWithLabel(dbls)[dbl];
	
	if(labelsTempUpToDate)
		_labelsTempRevision = _revision;
	
	return label;
}

bool Column::replaceDoubleLabelFromRowWithDouble(size_t row, double dbl)
{
	if(row < _labels.size())
	{
		bool willWork = _labels[row]->originalValue().isDouble() && _labels[row]->labelDisplay() == _labels[row]->originalValueAsString(false);
		if(willWork)
		{
			_labels[row]			-> setOriginalValue(	dbl	);
			_labels[row]			-> setLabel(			_labels[row]->originalValueAsString(false));
			_labelsTemp[row]		=  _labels[row]->label();
			_labelsTempDbls[row]	=  dbl;
		}
		
		return willWork;
	}
			
	double	originalDbl = _labelsTempDbls[row];
	if(!std::isnan(originalDbl))
		for(double & dblsRef : _dbls)
			if(Utils::isEqual(dblsRef, originalDbl))
				dblsRef = dbl;
	
	_labelsTempDbls[row]	=  dbl;
	_labelsTemp[row]		=  ColumnUtils::doubleToString(dbl);
	
	return true;
}

void Column::labelValueChanged(Label *label, double aDouble)
{
	//Lets assume that all occurences of a label in _dbls are the same.
	//So when we encounter one that is the same as what is passed here we can return immediately
	
	for(size_t r=0; r<_dbls.size(); r++)
		if(_ints[r] == label->intsId())
		{
			if(Utils::isEqual(_dbls[r], aDouble))
				return;
			
			_dbls[r] = aDouble;
		}
	
	dbUpdateValues();
}

void Column::labelDisplayChanged(Label *label)
{
	if(_labelsTempRevision < _revision)
		return; //We dont care about this change anymore if the list is out of date

	size_t labelIdx = labelIndex(label);
	
	if(_labelsTemp.size() > labelIdx)
		_labelsTemp[labelIdx] = label->label();
	
	//So we know that label is about to trigger an incRevision for the column through dbUpdate and checkForChanges
	_labelsTempRevision++;
}

Label * Column::labelByRow(int row) const
{
	if (row < rowCount() && _type != columnType::scale && _ints[row] != EmptyValues::missingValueInteger)
		return labelByIntsId(_ints[row]);

	return nullptr;
}

bool Column::setStringValueToRow(size_t row, const std::string & userEntered)
{
    JASPTIMER_SCOPE(Column::setStringValueToRowIfItFits);
    
	if(userEntered == "")
	{
		if (getValue(row) == "")	return false;
		else						return setValue(row, EmptyValues::missingValueDouble);
	}
	
	double	newDoubleToSet	= EmptyValues::missingValueDouble,
			oldDouble		= _dbls[row];	
	bool	itsADouble		= ColumnUtils::getDoubleValue(userEntered, newDoubleToSet);
	Label * newLabel		= labelByDisplay(userEntered);
	Label * oldLabel		= _ints[row] == Label::DOUBLE_LABEL_VALUE ? nullptr : labelByIntsId(_ints[row]);
		
	if(!oldLabel && !newLabel && itsADouble) //no labels and it is a double, easy peasy
		return setValue(row, newDoubleToSet);

	if(newLabel)
		return setValue(row, newLabel->intsId(), newDoubleToSet);
		
	if(itsADouble)
	{
		//There is no new label, an oldLabel AND we have a non-empty double in _dbls
		//This should mean that the label has this old double as a original value!
		if(	oldLabel
			&& !std::isnan(oldDouble)
			&& (	!oldLabel->originalValue().isDouble()
				|| !Utils::isEqual(_dbls[row], oldLabel->originalValue().asDouble())
				))
			Log::log() << "bool Column::setStringValueToRowIfItFits(" << row << ", '" << userEntered << "', ...) had differences between _dbls[" << row << "](" << _dbls[row] << ") and oldLabel originalValue: '" << oldLabel->originalValueAsString() << "'" << std::endl;
		
		return setValue(row, newDoubleToSet);
	}
	else
		//there is no new label yet for this and it is not a double so we need to make a label
		return setValue(row, labelsAdd(userEntered));
}

bool Column::setValue(size_t row, const std::string & value, const std::string & label)
{
	JASPTIMER_SCOPE(Column::setValue(size_t row, const std::string & value, const std::string & label));
    
	//If value != "" and label == "" that means we got copy pasted stuff in the viewer. And we just dont have labels, but we can treat it like we are editing
	//if both are "" we just want to clear the cell
	//the assumption is that this is not direct user-input, but internal jasp stuff.
	if(value == "" && label == "")
		return setValue(row, EmptyValues::missingValueDouble);
	
	if(label == "")
		return setStringValueToRow(row, value);
	
	bool	labelIsValue	= value == label;
	double	newDoubleToSet	= EmptyValues::missingValueDouble,
			oldDouble		= _dbls[row];	
	bool	itsADouble		= ColumnUtils::getDoubleValue(value, newDoubleToSet);
	Label * newLabel		= labelByValueAndDisplay(value, label);
	Label * oldLabel		= _ints[row] == Label::DOUBLE_LABEL_VALUE ? nullptr : labelByIntsId(_ints[row]);
	
	if(!newLabel && value != label) //no new label found but value and label are different. Given that this exact combination does not occur we add a new label
		newLabel = labelByIntsId( labelsAdd(label, "", itsADouble ? Json::Value(newDoubleToSet) : value));
		
	if(!oldLabel && !newLabel && itsADouble) //no labels and it is a double, easy peasy
		return setValue(row, newDoubleToSet);

	if(newLabel)
		return setValue(row, newLabel->intsId(), newDoubleToSet);
		
	if(itsADouble && labelIsValue)
	{
		//There is no new label, an oldLabel AND we have a non-empty double in _dbls
		//This should mean that the label has this old double as a original value!
		if(	oldLabel
			&& !std::isnan(oldDouble)
			&& (	!oldLabel->originalValue().isDouble()
				|| !Utils::isEqual(_dbls[row], oldLabel->originalValue().asDouble())
				))
			Log::log() << "bool Column::setValue(" << row << ", '" << value << "', '" << label << "') had differences between _dbls[" << row << "](" << _dbls[row] << ") and oldLabel originalValue: '" << oldLabel->originalValueAsString() << "'" << std::endl;
		
		return setValue(row, newDoubleToSet);
	}
	else
		//there is no new label yet for this and it is not a double so we need to make a label
		return setValue(row, labelsAdd(label, "", itsADouble ? Json::Value(newDoubleToSet) : value));
}

bool Column::setValue(size_t row, int value, bool writeToDB)
{
	JASPTIMER_SCOPE(Column::setValue int);
	return setValue(row, value, EmptyValues::missingValueDouble, writeToDB);
}

bool Column::setValue(size_t row, double value, bool writeToDB)
{
	JASPTIMER_SCOPE(Column::setValue double);
	return setValue(row, Label::DOUBLE_LABEL_VALUE, value, writeToDB);
}

bool Column::setValue(size_t row, int valueInt, double valueDbl, bool writeToDB)
{
	JASPTIMER_SCOPE(Column::setValue double);
	
	if(row >= _dbls.size())
		return false;
	
	bool changed = !Utils::isEqual(_dbls[row], valueDbl) || _ints[row] != valueInt;
	
	_dbls[row] = valueDbl;
	_ints[row] = valueInt;
	
	if(writeToDB)
	{
		db().columnSetValue(_id, row, valueInt, valueDbl);
		incRevision(false);
	}
	
	return changed;
}

void Column::rowInsertEmptyVal(size_t row)
{
	_dbls.insert(_dbls.begin() + row, EmptyValues::missingValueDouble);
	_ints.insert(_ints.begin() + row, EmptyValues::missingValueInteger);
}

void Column::rowDelete(size_t row)
{
	_dbls.erase(_dbls.begin() + row);
	_ints.erase(_ints.begin() + row);
}

void Column::setRowCount(size_t rows)
{
	_dbls.resize(rows);
	_ints.resize(rows);
}

Label *Column::labelByIntsId(int value) const
{
	JASPTIMER_SCOPE(Column::labelByValue);

	return _labelByIntsIdMap.count(value) ? _labelByIntsIdMap.at(value) : nullptr;
}

Label *Column::labelByDisplay(const std::string & display) const
{
	JASPTIMER_SCOPE(Column::labelByDisplay);

	for(Label * label : _labels)
		if(label->label() == display)
			return label;

	return nullptr;
}

Label *Column::labelByValue(const std::string & value) const
{
	JASPTIMER_SCOPE(Column::labelByValue);

	for(Label * label : _labels)
		if(label->originalValueAsString() == value)
			return label;

	return nullptr;
}

Label *Column::labelByValueAndDisplay(const std::string &value, const std::string &labelText) const
{
	JASPTIMER_SCOPE(Column::labelByValueAndDisplay);

	for(Label * label : _labels)
		if(label->originalValueAsString() == value && label->label() == labelText)
			return label;

	return nullptr;
}

int Column::labelIndex(const Label *label) const
{
	for(size_t i=0; i<_labels.size(); i++)
		if(_labels[i] == label)
			return i;
	return -1;
}

std::set<size_t> Column::labelsMoveRows(std::vector<qsizetype> rows, bool up)
{
	JASPTIMER_SCOPE(Column::labelsMoveRows);
	
	int mod = up ? -1 : 1;

	std::sort(rows.begin(), rows.end(), [&](const auto & l, const auto & r) { return up ? l < r : r < l; });
	
	replaceDoublesTillLabelsRowWithLabels(std::min(qsizetype(labelsTempCount() - 1), rows.back() + 1));
	
	std::vector<Label*> new_labels(_labels.begin(), _labels.end());

	for (size_t row : rows)
		if(int(row) + mod < 0 || int(row) + mod >= int(labelsTempCount()))
			return {}; //Because we can't move *out* of our _labels for obvious reasons

	std::set<size_t> rowsChanged;

	for (size_t row : rows)
	{
		std::iter_swap(new_labels.begin() + row, new_labels.begin() + (row + mod));
		rowsChanged.insert(row);
		rowsChanged.insert(row + mod);
	}

	_labels = new_labels;
	
	labelsTempReset();
	_dbUpdateLabelOrder();

	return rowsChanged;
}

void Column::labelsReverse()
{
	JASPTIMER_SCOPE(Column::labelsReverse);
	
	replaceDoublesTillLabelsRowWithLabels(labelsTempCount()-1);
	std::reverse(_labels.begin(), _labels.end());
	
	labelsTempReset();
	_dbUpdateLabelOrder();
}


DatabaseInterface & Column::db()
{
	return _data->db();
}

const DatabaseInterface  & Column::db() const
{
	return _data->db();
}

bool Column::allLabelsPassFilter() const
{
	for(const Label * label : _labels)
		if(!label->filterAllows())
			return false;
	return true;
}

bool Column::hasFilter() const
{
	return !allLabelsPassFilter();
}


void Column::resetFilter()
{
	db().transactionWriteBegin();
	
	for(Label * label : _labels)
		label->setFilterAllows(true);
	
	incRevision();

	db().transactionWriteEnd();
}

void Column::incRevision(bool labelsTempCanBeMaintained)
{
	assert(_id != -1);

	if(!_data->writeBatchedToDB())
	{
		bool setLabelsTempRevision = labelsTempCanBeMaintained && _revision == _labelsTempRevision;
		
		_revision = db().columnIncRevision(_id);
		
		if(setLabelsTempRevision)
			_labelsTempRevision = _revision;
		
		checkForChanges();
	}
}

bool Column::checkForUpdates()
{
	assert(_id != -1);

	if(_revision == db().columnGetRevision(_id))
		return false;

	dbLoad();
	return true;
}

bool Column::isColumnDifferentFromStringValues(const stringvec & strVals) const 
{
	return strVals == valuesAsStrings();
}

void Column::upgradeSetDoubleLabelsInInts()
{
	_ints = intvec(_dbls.size(), Label::DOUBLE_LABEL_VALUE);
	
	dbUpdateValues();
}

void Column::upgradeExtractDoublesIntsFromLabels()
{
	_dbls.resize(_ints.size());
	
	for(qsizetype r=0; r<_dbls.size(); r++)
	{
		Label * label = labelByIntsId(_ints[r]);
		
		_dbls [r] = ! label || !label->originalValue().isDouble() ? EmptyValues::missingValueDouble : label->originalValue().asDouble();
	}
	
	dbUpdateValues();
}

void Column::checkForLoopInDependencies(std::string code)
{
	stringset dependencies	= _data->findUsedColumnNames(code);
	
	if(dependencies.count(name()) > 0)
		throw std::logic_error("A computed column can not use itself!");

	std::set<std::string> foundNames	= { name() };

	for(Column * col : _data->columns())
		if(col->isComputed() && dependencies.count(col->name()) > 0)
			col->_checkForDependencyLoop(foundNames, std::list<std::string>({name()}) );
}

void Column::_checkForDependencyLoop(stringset foundNames, std::list<std::string> loopList)
{
	if(foundNames.count(name()) > 0)
	{
		std::stringstream loopSS;

		loopSS << "In the definitions of your computed columns the following loop was found:\n";
		for(const std::string & l : loopList)
			loopSS << l << " depends on ";
		loopSS << name() << "..\nThis is not allowed, so change one of the formulas to break the circle.";

		throw std::logic_error(loopSS.str());
	}

	foundNames.insert(name());
	std::list<std::string> superLoopList(loopList);
	superLoopList.push_back(name());

	findDependencies();

	for(Column * col : _data->columns())
		if(col->isComputed() && _dependsOnColumns.count(col->name()) > 0)
			col->_checkForDependencyLoop(foundNames, superLoopList);
}

void Column::setDependsOn(const stringset & cols)
{
	_dependsOnColumns = cols;	
}

void Column::findDependencies()
{
	if(codeType() != computedColumnType::analysis)
		_dependsOnColumns = _data->findUsedColumnNames(rCodeStripped());
}

const stringset & Column::dependsOnColumns(bool refresh)
{
	if(refresh)
		findDependencies();
	
	return _dependsOnColumns;
}

Json::Value Column::serialize() const
{
	Json::Value json(Json::objectValue);

	json["name"]			= _name;
	json["title"]			= _title;
	json["description"]		= _description;
	json["rCode"]			= _rCode;
	json["type"]			= int(_type);
	json["analysisId"]		= _analysisId;
	json["invalidated"]		= _invalidated;
	json["codeType"]		= int(_codeType);
	json["error"]			= _error;
	json["rCode"]			= _rCode;
	json["constructorJson"] = _constructorJson;

	Json::Value jsonLabels(Json::arrayValue);
	for (const Label* label : _labels)
		jsonLabels.append(label->serialize());

	Json::Value jsonDbls(Json::arrayValue);
	for (double dbl : _dbls)
		jsonDbls.append(dbl);

	Json::Value jsonInts(Json::arrayValue);
	for (int i : _ints)
		jsonInts.append(i);

	json["customEmptyValues"]	= _emptyValues->toJson();

	json["labels"]				= jsonLabels;
	json["dbls"]				= jsonDbls;
	json["ints"]				= jsonInts;

	return json;
}

void Column::deserialize(const Json::Value &json)
{
	if (json.isNull())
		return;

	std::string name = json["name"].asString(),
				title = json["title"].asString();

	_name				= getUniqueName(name);
	db().columnSetName(_id, _name);

	// If title was equal to name, then they should still stay the same if the name is changed to be unique.
	_title				= name == title ? _name : title;
	db().columnSetTitle(_id, _title);

	_description		= json["description"].asString();
	db().columnSetDescription(_id, _description);

	_type				= columnType(json["type"].asInt());
	db().columnSetType(_id, _type);

	_invalidated		= json["invalidated"].asBool();
	_codeType			= computedColumnType(json["codeType"].asInt());
	_rCode				= json["rCode"].asString();
	_error				= json["error"].asString();
	_constructorJson	= json["constructorJson"];
	_analysisId			= json["analysisId"].asInt();

	db().columnSetComputedInfo(_id, _analysisId, _invalidated, _codeType, _rCode, _error, constructorJsonStr());
	
	for (Label* label : _labels)
	{
		_labelByIntsIdMap.erase(label->intsId());
		label->dbDelete();
		delete label;
	}
	_labelByIntsIdMap.clear();
	_labels.clear();

	const Json::Value& labels = json["labels"];
	if (labels.isArray())
	{
		for (const Json::Value& labelJson : labels)
			labelsAdd(labelJson["value"].asInt(), labelJson["label"].asString(), labelJson["filterAllows"].asBool(), labelJson["description"].asString(), labelJson["originalValue"].asString(), labelJson["order"].asInt(), -1);
	}

	_emptyValues->fromJson(json["customEmptyValues"]);
	
	size_t i=0;
	_dbls.resize(json["dbls"].size());
	for (const Json::Value& dblJson : json["dbls"])
		_dbls[i++] = dblJson.asDouble();
	
	i=0;
	_ints.resize(json["ints"].size());
	for (const Json::Value& intJson : json["ints"])
		_ints[i++] = intJson.asInt();
	
	assert(_ints.size() == _dbls.size());
	
	dbUpdateValues();
}

std::string Column::getUniqueName(const std::string &name) const
{
	std::string result	= name;
	int	suffix			= 1;
	bool foundSameName	= false;

	do
	{
		foundSameName	= false;
		for(Column * col : _data->columns())
		{
			if (col != this && col->name() == result)
			{
				foundSameName = true;
				break;
			}
		}

		if (foundSameName)
		{
			suffix++;
			result = name + " " + std::to_string(suffix);
		}

	} while (foundSameName);

	return result;
}

bool Column::dependsOn(const std::string & columnName, bool refresh)
{
	return dependsOnColumns(refresh).count(columnName) > 0;
}

bool Column::isComputedByAnalysis(size_t analysisID)
{
	return isComputed()	&&	codeType() == computedColumnType::analysis &&  analysisId() == analysisID;
}

bool Column::isEmptyValue(const std::string& val) const
{
	return _emptyValues->isEmptyValue(val);
}

bool Column::isEmptyValue(const double val) const
{
	return _emptyValues->isEmptyValue(val);
}

qsizetype Column::getMaximumWidthInCharacters(bool shortenAndFancyEmptyValue, bool valuesPlease)
{
	qsizetype	extraPad	= 4,
				maxWidth	= 0;
	std::string takeWidth;
	
	//Call labelsTempCount() to both find out how many there are and generate them if necessary
	bool thereAreLabels = labelsTempCount() > 0;
	
	if(thereAreLabels)
		for(Label * label : labels())
		{
			takeWidth	= !valuesPlease ? label->label(shortenAndFancyEmptyValue) : label->originalValueAsString(shortenAndFancyEmptyValue);
			maxWidth	= std::max(maxWidth, qsizetype(stringUtils::approximateVisualLength(takeWidth)));
		}
	
	
	return std::max(maxWidth, _labelsTempMaxWidth) + extraPad;
}


