#include "column.h"
#include "dataset.h"
#include "log.h"
#include "columnutils.h"
#include "databaseinterface.h"

Column::Column(DataSet * data, int id) 
	: DataSetBaseNode(dataSetBaseNodeType::column, data->dataNode()), _data(data), _id(id)
{
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

	assert(_id != -1 || id != -1);
	if(id != -1)
		_id = id;

	db().transactionReadBegin();
	
					db().columnGetBasicInfo(	_id, _name, _title, _description, _type, _revision);
	_isComputed =   db().columnGetComputedInfo(	_id, _analysisId, _invalidated, _codeType, _rCode, _error, _constructorJson);

	db().labelsLoad(this);
	
	if(getValues)
	{
		if(type() == columnType::scale)	db().columnGetValuesDbls(_id, _dbls);
		else							db().columnGetValuesInts(_id, _ints);
	}

	db().transactionReadEnd();
}

void Column::dbLoadIndex(int index, bool getValues)
{
	JASPTIMER_SCOPE(Column::dbLoadIndex);

	dbLoad(db().columnIdForIndex(_data->id(), index), getValues);
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
	return _data->hasCustomEmptyValues(_name);
}

const stringset& Column::emptyValues() const
{
	return _data->emptyValues(_name);
}

const doubleset& Column::doubleEmptyValues() const
{
	return _data->doubleEmptyValues(_name);
}

void Column::setHasCustomEmptyValues(bool hasCustom)
{
	JASPTIMER_SCOPE(Column::setHasCustomEmptyValues);

	if (_data->hasCustomEmptyValues(_name) == hasCustom)
		return;

	_data->setHasCustomEmptyValues(_name, hasCustom);

	incRevision();
}

void Column::setCustomEmptyValues(const stringset& customEmptyValues)
{
	JASPTIMER_SCOPE(Column::setCustomEmptyValues);

	if (hasCustomEmptyValues() && emptyValues() == customEmptyValues)
		return;

	_data->setCustomEmptyValues(_name, customEmptyValues);

	incRevision();
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
	incRevision();
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

	_codeType = codeType;
	_isComputed = _codeType != computedColumnType::notComputed;

	
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

void Column::setIsComputed(bool isComputed)
{
	JASPTIMER_SCOPE(Column::setIsComputed);

	if(_isComputed == isComputed)
		return;
	
	_isComputed = isComputed;
	
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

	_isComputed			= true;
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
		vals.insert(label->value());
	
	return vals;
}

void Column::_convertVectorIntToDouble(intvec & intValues, doublevec & doubleValues)
{
	JASPTIMER_SCOPE(Column::_convertVectorIntToDouble);

	doubleValues.clear();
	for (int intValue : intValues)
		doubleValues.push_back(intValue == std::numeric_limits<int>::lowest() ? NAN : double(intValue));
}

bool Column::_resetMissingDataForNominal(intstrmap &missingDataMap)
{
	JASPTIMER_SCOPE(Column::_resetMissingDataForNominal);

	intstrmap	emptyValuesMapOrg	= missingDataMap;
	bool		hasChanged			= false,
				hasMissingData		= !missingDataMap.empty(),
				changeToNominalText = false;
	int			row					= 0;

	intset uniqueValues = getUniqueLabelValues();

	for (int intValue : _ints)
	{
		if (intValue == std::numeric_limits<int>::lowest() && hasMissingData)
		{
			if(missingDataMap.count(row))
			{
				std::string orgValue = missingDataMap[row];
				if (!isEmptyValue(orgValue))
				{
					// This value is not empty anymore
					if (ColumnUtils::getIntValue(orgValue, intValue))
					{
						_ints[row] = intValue;
						uniqueValues.insert(intValue);
						hasChanged = true;
						missingDataMap.erase(row);
					}
					else
					{
						// The original value is not an integer, this column cannot be nominal anymore
						// Let's make it a nominal text.
						changeToNominalText = true;
						break;
					}
				}
			}
		}
		else if (intValue != std::numeric_limits<int>::lowest() && isEmptyValue(intValue))
		{
			// This value is now considered as empty
			_ints[row] = std::numeric_limits<int>::lowest();
			uniqueValues.erase(intValue);
			hasChanged = true;
			missingDataMap.insert(make_pair(row, std::to_string(intValue)));
		}
		row++;
	}

	if (changeToNominalText)
	{
		setType(columnType::nominalText);
		missingDataMap.clear();
		missingDataMap.insert(emptyValuesMapOrg.begin(), emptyValuesMapOrg.end());
		hasChanged = _resetMissingDataForNominalText(missingDataMap, false);
	}
	else if (hasChanged)
		labelsSyncInts(uniqueValues);

	if(hasChanged)
	{
		if(!_data->writeBatchedToDB())
			db().columnSetValues(_id, _ints);
		incRevision();
	}

	return hasChanged;
}

bool Column::_resetMissingDataForScale(intstrmap & missingDataMap)
{
	JASPTIMER_SCOPE(Column::_resetMissingDataForScale);

	bool	hasChanged			= false,
			hasMissingData		= !missingDataMap.empty(),
			changeToNominalText = false;
	int		row					= 0;
	
	for (double doubleValue : _dbls)
	{
		if (std::isnan(doubleValue) && hasMissingData)
		{
			if(missingDataMap.count(row))
			{
				std::string orgValue = missingDataMap[row];
				if (!isEmptyValue(orgValue))
				{
					// This value is not empty anymore
					if (ColumnUtils::getDoubleValue(orgValue, doubleValue))
					{
						_dbls[row] = doubleValue;
						hasChanged = true;
					}
					else
					{
						changeToNominalText = true;
						break;
					}
				}
			}
		}
		else if (!std::isnan(doubleValue) && isEmptyValue(doubleValue))
		{
			// This value is now considered as empty
			_dbls[row] = NAN;
			hasChanged = true;
			missingDataMap.insert(make_pair(row, ColumnUtils::doubleToString(doubleValue)));
		}
		row++;
	}

	if (changeToNominalText)
	{
		// Cannot use _resetMissingDataForNominalText since the AsInts are not set.
		// So use setColumnAsNominalText
		stringvec values;
		for(size_t row=0; row<_dbls.size(); row++)
			if (std::isnan(_dbls[row]))	values.push_back(missingDataMap.count(row) ? missingDataMap[row] : "");
			else						values.push_back(std::to_string(_dbls[row]));
			

		intstrmap newEmptyValues = setAsNominalText(values);
		missingDataMap.clear();
		missingDataMap.insert(newEmptyValues.begin(), newEmptyValues.end());
		hasChanged = true;
	}
	else if(hasChanged)
	{
		db().columnSetValues(_id, _dbls);
		incRevision();
	}

	return hasChanged;
}

//This function is pretty hard to read...
bool Column::_resetMissingDataForNominalText(intstrmap & missingDataMap, bool tryToConvert)
{
	JASPTIMER_SCOPE(Column::_resetMissingDataForNominalText);

	bool				hasChanged					= false;
	bool				hasMissingData				= !missingDataMap.empty(),
						canBeConvertedToIntegers	= tryToConvert,
						canBeConvertedToDoubles		= tryToConvert;
	stringvec			values;
	intvec				intValues;
	doublevec			doubleValues;
	intset				uniqueIntValues;
	intstrmap			intLabels;

	for(size_t row =0; row<_ints.size(); row++)
	{
		int key = _ints[row];
		
		if (key == std::numeric_limits<int>::lowest() && hasMissingData)
		{
			//currently this row has empty value, but maybe with the new map no more?
			if(missingDataMap.count(row))
			{
				std::string orgValue = missingDataMap[row];
				values.push_back(orgValue);

				if (!isEmptyValue(orgValue))
					hasChanged = true; //So this emptyValue became a normal value!

				if (canBeConvertedToIntegers || canBeConvertedToDoubles)
				{
					if (isEmptyValue(orgValue))
					{
						if (canBeConvertedToIntegers)	intValues.push_back(std::numeric_limits<int>::lowest());
						else							doubleValues.push_back(NAN);
					}
					else
					{
						bool convertToDouble = false;

						if (!canBeConvertedToIntegers)
							convertToDouble = true;
						else
						{
							int intValue;

							if (ColumnUtils::getIntValue(orgValue, intValue))
							{
								intValues.push_back(intValue);
								if (!uniqueIntValues.count(intValue))
								{
									uniqueIntValues.insert(intValue);
									intLabels.insert(make_pair(intValue, orgValue));
								}
								missingDataMap.erase(row);
							}
							else
							{
								canBeConvertedToIntegers = false;
								_convertVectorIntToDouble(intValues, doubleValues);
								convertToDouble = true;
							}
						}


						if (convertToDouble)
						{
							double doubleValue;
							if (ColumnUtils::getDoubleValue(orgValue, doubleValue))
							{
								doubleValues.push_back(doubleValue);
								missingDataMap.erase(row);
							}
							else
								canBeConvertedToDoubles = false;
						}
					}
				}
			}
			else //if we couldnt find the "row" in the emptyValuesMap?
			{
				values.push_back("");

				if (canBeConvertedToIntegers)		intValues.push_back(std::numeric_limits<int>::lowest());
				else if (canBeConvertedToDoubles)	doubleValues.push_back(NAN);
			}
		}
		else if (key == std::numeric_limits<int>::lowest())
		{
			//Didnt have emptyValues so itll stay empty
			values.push_back("");

			if (canBeConvertedToIntegers)		intValues.push_back(std::numeric_limits<int>::lowest());
			else if (canBeConvertedToDoubles)	doubleValues.push_back(NAN);
		}
		else
		{
			//We have a value here, but it might not be full anymore
			Label		*	labelHere	= labelByValue(key);
			std::string		orgValue	= labelHere->originalValueAsString();
			values.push_back(orgValue);

			if (isEmptyValue(orgValue))
			{
				hasChanged = true;
				if (canBeConvertedToIntegers)
				{
					intValues.push_back(std::numeric_limits<int>::lowest());
					missingDataMap.insert(make_pair(row, orgValue));
				}
				else if (canBeConvertedToDoubles)
				{
					doubleValues.push_back(NAN);
					missingDataMap.insert(make_pair(row, orgValue));
				}
			}
			else
			{
				bool convertToDouble = false;
				if (canBeConvertedToIntegers)
				{
					int intValue;
					if (ColumnUtils::getIntValue(orgValue, intValue))
					{
						intValues.push_back(intValue);
						if (!uniqueIntValues.count(intValue))
						{
							uniqueIntValues.insert(intValue);
							intLabels.insert(make_pair(intValue, labelHere->label()));
						}
					}
					else
					{
						canBeConvertedToIntegers = false;
						_convertVectorIntToDouble(intValues, doubleValues);
						convertToDouble = true;
					}
				}
				else if (canBeConvertedToDoubles)
					convertToDouble = true;

				if (convertToDouble)
				{
					double doubleValue;
					if (ColumnUtils::getDoubleValue(orgValue, doubleValue))	doubleValues.push_back(doubleValue);
					else													canBeConvertedToDoubles = false;
				}
			}
		}
	}
	// End of int for loop

	//Ok, so now we have checked the "original values" for being convertible to doubles or ints, pretty awesome.
	//But there are scenarios where the user added some nice labels to their doubles (SPSS can have labels on doubles) or ints and right now these would just be thrown away.
	//Lets check if the labels are maybe not convertible to whatever datatype all orginal values *can* be.

	if(canBeConvertedToIntegers)
		for(const Label * label : _labels)
			if(!ColumnUtils::isIntValue(label->label()))
			{
				canBeConvertedToIntegers = false;
				break;
			}

	if(canBeConvertedToDoubles)
		for(const Label * label : _labels)
			if(!ColumnUtils::isDoubleValue(label->label()))
			{
				canBeConvertedToDoubles = false;
				break;
			}

	//Now we haven't even checked if the labels are maybe different from the values stored in orginalString... So we might still be throwing info away
	//With the new database storage this is no longer necessarily the case
	if (canBeConvertedToIntegers)
	{
		labelsClear();
		setAsNominalOrOrdinal(intValues, intLabels);
		hasChanged = true;
	}
	else if (canBeConvertedToDoubles)
	{
		setAsScale(doubleValues);
		hasChanged = true;
	}
	else if (hasChanged)
	{
		intstrmap newEmptyValues = setAsNominalText(values);
		missingDataMap.clear();
		missingDataMap.insert(newEmptyValues.begin(), newEmptyValues.end());
	}

	return hasChanged;

}


bool Column::resetMissingData(intstrmap &missingDataMap)
{
	JASPTIMER_SCOPE(Column::resetEmptyValues);

	switch(_type)
	{
	case columnType::ordinal:
	case columnType::nominal:	return _resetMissingDataForNominal(		missingDataMap);
	case columnType::scale:		return _resetMissingDataForScale(		missingDataMap);
	default:					return _resetMissingDataForNominalText(	missingDataMap);
	}
}

columnTypeChangeResult Column::_changeColumnToNominalOrOrdinal(enum columnType newColumnType)
{
	JASPTIMER_SCOPE(Column::_changeColumnToNominalOrOrdinal);

	if (_type == columnType::nominal || _type == columnType::ordinal)
	{
		_type = newColumnType;
		return columnTypeChangeResult::changed;
	}

	if (_type == columnType::nominalText)
	{
		intvec		values;
		intset		uniqueIntValues;
		intstrmap	intLabels;

		for (int key : ints())
		{
			int intValue = std::numeric_limits<int>::lowest();

			if (key != std::numeric_limits<int>::lowest())
			{
				std::string value = labelByValue(key)->originalValueAsString();
				if (!isEmptyValue(value) && !ColumnUtils::getIntValue(value, intValue))
					break;
			}

			values.push_back(intValue);

			if (intValue != std::numeric_limits<int>::lowest())
			{
				if (uniqueIntValues.find(intValue) == uniqueIntValues.end())
				{
					std::string label = _getLabelDisplayStringByValue(key);
					uniqueIntValues.insert(intValue);
					intLabels.insert(make_pair(intValue, label));
				}
			}
		}

		if (values.size() == rowCount())
		{
			labelsClear();
			setAsNominalOrOrdinal(values, intLabels, newColumnType == columnType::ordinal);
			return columnTypeChangeResult::changed;
		}

		// nominalText to nominal: we could not make the values as integers, but
		// the column can still stay a NominalText, so it is not a failure.
		return newColumnType == columnType::nominal ? columnTypeChangeResult::changed : columnTypeChangeResult::cannotConvertStringValueToInteger;
	}
	else if (_type == columnType::scale)
	{
		std::vector<int>	values;

		for (double doubleValue : dbls())
		{
			int intValue = std::numeric_limits<int>::lowest();

			if (!isEmptyValue(doubleValue) && !ColumnUtils::getIntValue(doubleValue, intValue))
				break;

			values.push_back(intValue);
		}

		if (values.size() == rowCount())
		{
			setAsNominalOrOrdinal(values, newColumnType == columnType::ordinal);
			return columnTypeChangeResult::changed;
		}
		else if (newColumnType == columnType::nominal)
		{
			stringvec values;

			for (double doubleValue : dbls())
				if (std::isnan(doubleValue))	values.push_back("");
				else							values.push_back(ColumnUtils::doubleToString(doubleValue));

			setAsNominalText(values);
			return columnTypeChangeResult::changed;
		}

		//Because newColumnType != nominal and values.size() != rowCount that means we were trying to convert to Ordinal, but we couldnt convert all scalar values to integers.
		return columnTypeChangeResult::cannotConvertDoubleValueToInteger;
	}

	return columnTypeChangeResult::unknownError;
}

columnTypeChangeResult Column::_changeColumnToScale()
{
	std::vector<double> values;

	switch(_type)
	{
	case columnType::nominal:
	case columnType::ordinal:
		for (int intValue : ints())
		{
			double doubleValue = NAN;

			bool	converted	= false;

			if (intValue != std::numeric_limits<int>::lowest() && labelByValue(intValue))
			{
				std::string value = labelByValue(intValue)->originalValueAsString();
				if (!isEmptyValue(value))
					converted = ColumnUtils::getDoubleValue(value, doubleValue);
			}

			if (!converted && intValue != std::numeric_limits<int>::lowest() && !isEmptyValue(intValue))
				doubleValue = double(intValue);

			values.push_back(doubleValue);
		}
		break;

	case columnType::nominalText:

		for (int key : ints())
		{
			double	doubleValue = NAN;
			bool	converted	= false;

			if (key != std::numeric_limits<int>::lowest())
			{
				std::string value = labelByValue(key)->originalValueAsString();
				if (!isEmptyValue(value))
					converted = ColumnUtils::getDoubleValue(value, doubleValue);
			}
			else
				converted = true; //Because if key == std::numeric_limits<int>::lowest() then it is missing value

			if (converted)	values.push_back(doubleValue);
			else			return columnTypeChangeResult::cannotConvertStringValueToDouble;
		}
		break;

	default:
		break;
	}

	setAsScale(values);
	return columnTypeChangeResult::changed;
}

columnTypeChangeResult Column::changeType(columnType colType)
{
	JASPTIMER_SCOPE(Column::changeType);
	
	if(!isComputed())
	{
		if (colType == _type)				
			return columnTypeChangeResult::changed;
		else
		{
			columnTypeChangeResult changeResult = colType == columnType::scale ? _changeColumnToScale() : _changeColumnToNominalOrOrdinal(colType);

			if(changeResult == columnTypeChangeResult::changed && _type != columnType::nominalText && _preEditType != _type)
				_preEditType = _type;

			return changeResult;
		}
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


bool Column::initAsScale(size_t colNo, std::string newName, const doublevec & values)
{
	JASPTIMER_SCOPE(Column::initAsScale);

	setName(newName);
	db().columnSetIndex(_id, colNo);

	return setAsScale(values);
}


intstrmap Column::initAsNominalText(size_t colNo, std::string newName, const stringvec & values, const strstrmap & labels)
{
	JASPTIMER_SCOPE(Column::initAsNominalText);

	setName(newName);
	
	return setAsNominalText(values, labels);
}

bool Column::initAsNominalOrOrdinal(size_t colNo, std::string newName, const intvec & values, bool is_ordinal)
{
	JASPTIMER_SCOPE(Column::initAsNominalOrOrdinal);

	setName(newName);

	return setAsNominalOrOrdinal(values, is_ordinal);
}

bool Column::initAsNominalOrOrdinal(size_t colNo, std::string newName, const intvec & values, const intstrmap &uniqueValues, bool is_ordinal)
{
	JASPTIMER_SCOPE(Column::initAsNominalOrOrdinal);

	setName(newName);

	return setAsNominalOrOrdinal(values, uniqueValues, is_ordinal);
}


void Column::setDefaultValues(enum columnType columnType)
{
	JASPTIMER_SCOPE(Column::setDefaultValues);

	if(columnType == columnType::unknown)
		columnType = _type;

	switch(columnType)
	{
	case columnType::scale:			setAsScale(				std::vector<double>(		rowCount(),		static_cast<double>(std::nanf(""))));			break;
	case columnType::ordinal:		setAsNominalOrOrdinal(	std::vector<int>(			rowCount(),		std::numeric_limits<int>::lowest()), true);		break;
	case columnType::nominal:		setAsNominalOrOrdinal(	std::vector<int>(			rowCount(),		std::numeric_limits<int>::lowest()), false);	break;
	case columnType::nominalText:	setAsNominalText(		std::vector<std::string>(	rowCount()),	{});											break;
	case columnType::unknown:		throw std::runtime_error("Trying to set default values of a column with unknown column type...");
	}
	labelsClear();
}

bool Column::setAsScale(const doublevec & values)
{
	JASPTIMER_SCOPE(Column::setAsScale);

	bool changedSomething = type() != columnType::scale;

	if(values.size() != _dbls.size())
	{
		changedSomething = true;
		if(_dbls.size() < values.size())
			_dbls.resize(values.size());
	}

	{
		size_t i=0;

		for(double value : values)
		{
			//Apparently checking a double can lead to a problem if they are both nan because nan != nan -> true
			bool valChanged = (std::isnan(_dbls[i]) != std::isnan(value));

			if(!valChanged && !std::isnan(value)) //So if they are equally nan and one is not nan they must both have a sensible value which can be compared and otherwise there was already a change
				valChanged = _dbls[i] != value;
			
			_dbls[i] = value;

			if(valChanged)
				changedSomething = true;

			i++;
		}

		for (; i < _dbls.size(); i++)
			_dbls[i] = NAN;
	}

	setType(columnType::scale);
	labelsClear(); //delete now unused labels so they can not be erroneously reused when returning to non-scalar type
	if(!_data->writeBatchedToDB())
		db().columnSetValues(_id, _dbls);
	incRevision();

	_ints.clear(); //We can load these from the DB later if wanted

	return changedSomething;
}

bool Column::setAsNominalOrOrdinal(const intvec & values, bool is_ordinal)
{
	JASPTIMER_SCOPE(Column::setAsNominalOrOrdinal);

	std::set<int> uniqueValues(values.begin(), values.end());
	uniqueValues.erase(std::numeric_limits<int>::lowest());

	bool labelChanged	= labelsSyncInts(uniqueValues);
	bool dataChanged	= _setAsNominalOrOrdinal(values, is_ordinal);

	return labelChanged || dataChanged;
}

bool Column::setAsNominalOrOrdinal(const intvec &values, intstrmap uniqueValues, bool is_ordinal)
{
	JASPTIMER_SCOPE(Column::setAsNominalOrOrdinal);

	std::set<int> uniqueValuesData(values.begin(), values.end());
	uniqueValuesData.erase(std::numeric_limits<int>::lowest());

	std::stringstream labelsMissing;
	int cnt = 0;

	for(int uniqVal : uniqueValuesData)
		if(uniqueValues.count(uniqVal) == 0)
		{
			labelsMissing << (cnt++ > 0 ? ", " : "") << uniqVal;
			uniqueValues[uniqVal] = std::to_string(uniqVal);
		}

	if(cnt)
		Log::log() << "Setting column '" << name() << "' data to " << (is_ordinal ? "ordinal" : "nominal") << " but it was missing (a) label(s) for value(s) (" << labelsMissing.str() << "), adding them as label." << std::endl;

	bool	labelChanged	= labelsSyncIntsMap(uniqueValues),
			dataChanged		= _setAsNominalOrOrdinal(values, is_ordinal);

	return labelChanged || dataChanged;
}

bool Column::overwriteDataWithScale(doublevec scalarData)
{
	JASPTIMER_SCOPE(Column::overwriteDataWithScale);

	labelsClear();

	size_t setVals = scalarData.size();

	if(scalarData.size() != rowCount())
		scalarData.resize(rowCount());

	for(size_t setThis = setVals; setThis<scalarData.size(); setThis++)
		scalarData[setThis] = static_cast<double>(std::nanf(""));

	return setAsScale(scalarData);
}

bool Column::overwriteDataWithOrdinal(intvec ordinalData, intstrmap levels)
{
	JASPTIMER_SCOPE(Column::overwriteDataWithOrdinal);

	size_t setVals = ordinalData.size();

	if(ordinalData.size() != rowCount())
		ordinalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<ordinalData.size(); setThis++)
		ordinalData[setThis] = std::numeric_limits<int>::lowest();

	return setAsNominalOrOrdinal(ordinalData, levels, true);
}

bool Column::overwriteDataWithOrdinal(intvec ordinalData)
{
	JASPTIMER_SCOPE(Column::overwriteDataWithOrdinal);

	size_t setVals = ordinalData.size();

	if(ordinalData.size() != rowCount())
		ordinalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<ordinalData.size(); setThis++)
		ordinalData[setThis] = std::numeric_limits<int>::lowest();

	return setAsNominalOrOrdinal(ordinalData, true);
}

bool Column::overwriteDataWithNominal(intvec nominalData, intstrmap levels)
{
	JASPTIMER_SCOPE(Column::overwriteDataWithNominal);

	size_t setVals = nominalData.size();

	if(nominalData.size() != rowCount())
		nominalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<nominalData.size(); setThis++)
		nominalData[setThis] = std::numeric_limits<int>::lowest();

	return setAsNominalOrOrdinal(nominalData, levels, false);
}

bool Column::overwriteDataWithNominal(intvec nominalData)
{
	JASPTIMER_SCOPE(Column::overwriteDataWithNominal);

	size_t setVals = nominalData.size();

	if(nominalData.size() != rowCount())
		nominalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<nominalData.size(); setThis++)
		nominalData[setThis] = std::numeric_limits<int>::lowest();

	return setAsNominalOrOrdinal(nominalData, false);
}

bool Column::overwriteDataWithNominal(stringvec nominalData)
{
	JASPTIMER_SCOPE(Column::overwriteDataWithNominal);

	if(nominalData.size() != rowCount())
		nominalData.resize(rowCount());

	bool changedSomething = false;
	setAsNominalText(nominalData, &changedSomething);

	return changedSomething;
}

bool Column::_setAsNominalOrOrdinal(const intvec &values, bool is_ordinal)
{
	JASPTIMER_SCOPE(Column::_setAsNominalOrOrdinal);

	bool changedSomething = type() != (is_ordinal ? columnType::ordinal : columnType::nominal);

	if(values.size() != _ints.size())
	{
		changedSomething = true;

		if(_ints.size() < values.size())
			_ints.resize(values.size());
	}

	{
		size_t i = 0;
		for(int value : values)
		{
			if(_ints[i] != value)
				changedSomething = true;
			
			_ints[i] = value;

			i++;
		}

		for (; i < _ints.size(); i++)
			_ints[i] = std::numeric_limits<int>::lowest();
	}

	_dbls.clear(); //We can load them later if needed

	setType(is_ordinal ? columnType::ordinal : columnType::nominal);
	if(!_data->writeBatchedToDB())
		db().columnSetValues(_id, _ints);
	
	if(changedSomething)
		incRevision();

	return changedSomething;
}

void Column::_dbUpdateLabelOrder()
{
	if(batchedLabel())
		return;

	intintmap orderPerDbIds;

	for(size_t i=0; i<_labels.size(); i++)
	{
		_labels[i]->setOrder(i);
		orderPerDbIds[_labels[i]->id()] = i;
	}

	db().labelsSetOrder(orderPerDbIds);
	incRevision();
}

void Column::_sortLabelsByOrder()
{
	std::sort(_labels.begin(), _labels.end(), [](const Label * l, const Label * r) { return l->order() < r->order(); });
}

intstrmap Column::setAsNominalText(const stringvec &values, const strstrmap & labels, bool * changedSomething)
{
	JASPTIMER_SCOPE(Column::setAsNominalText);

	if(changedSomething != nullptr)
		*changedSomething = type() != columnType::nominalText;
	
	intstrmap	emptyValuesMap;
	stringvec	unicifiedValues;
	stringset	foundAlready;

	//were not sorting here to keep the order of the labels the same as how we got them in the data, I guess this would make more sense then alphanumerical sort?

	for(const std::string & val : values)
		if(!foundAlready.count(val) && !isEmptyValue(val))
		{
			unicifiedValues.push_back(val);
			foundAlready.insert(val);
		}

	strintmap labelValueMap = labelsSyncStrings(unicifiedValues, labels, changedSomething);

	if(values.size() != _ints.size() && changedSomething != nullptr)
			*changedSomething = true;

	if(_ints.size() < values.size())
		_ints.resize(values.size());

	//Set the _ints values by iterating
	size_t i = 0;
	for(const std::string &value : values)
	{
		if (isEmptyValue(value))
		{
			if(changedSomething != nullptr && _ints[i] != std::numeric_limits<int>::lowest())
				*changedSomething = true;

			_ints[i] = std::numeric_limits<int>::lowest();

			if (!value.empty())
				emptyValuesMap.insert(make_pair(i, value));
		}
		else
		{
			if (labelValueMap.find(value) == labelValueMap.end())
			{
				std::string allLabels;
				for (auto const& imap: labelValueMap)
					allLabels += imap.first + ", ";
				throw std::runtime_error("Error when reading column " + name() + ": cannot convert value " + value + " to Nominal Text (all labels: " + allLabels + ")");
			}

			if(changedSomething != nullptr && _ints[i] != labelValueMap[value])
				*changedSomething = true;

			_ints[i] = labelValueMap[value];
		}

		i++;
	}

	for (; i < _ints.size(); i++)
		_ints[i] = std::numeric_limits<int>::lowest();

	_dbls.clear(); //We can load them later if needed

	setType(columnType::nominalText);
	if(!_data->writeBatchedToDB())
		db().columnSetValues(_id, _ints);
	incRevision();

	return emptyValuesMap;
}


void Column::labelsClear()
{
	db().labelsClear(_id);
	_labels.clear();
	_labelByValueMap.clear();

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

void Column::rememberOriginalColumnType()
{
	if(_preEditType == columnType::unknown)
		_preEditType = _type;
}


int Column::labelsAdd(int display)
{
	return labelsAdd(display, std::to_string(display), true, "", display);
}

int Column::labelsAdd(const std::string &display)
{
	if(display == "")
		return std::numeric_limits<int>::lowest();

	int newValue = 0;
	for(Label * label : _labels)
		if(newValue <= label->value())
			newValue = label->value() + 1;

	return labelsAdd(newValue, display, true, "", display);
}

int Column::labelsAdd(int value, const std::string & display, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order, int id)
{
	JASPTIMER_SCOPE(Column::labelsAdd);

	Label * label = new Label(this, display, value, filterAllows, description, originalValue, order, id);
	_labels.push_back(label);

	_labelByValueMap[label->value()] = label;

	_dbUpdateLabelOrder();
	return label->value();
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
				if(std::find(valuesToRemove.begin(), valuesToRemove.end(), label->value()) != valuesToRemove.end())
				{
					_labelByValueMap.erase(label->value());
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
	_labelByValueMap.clear();

	for (Label * label : _labels)
	{
		if (label->value() != labelValue)
			label->setValue(labelValue);

		result[label->label()] = labelValue;

		_labelByValueMap[labelValue] = label;

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

void Column::_resetLabelValueMap()
{
	_labelByValueMap.clear();
	for(Label * label : _labels)
		_labelByValueMap[label->value()] = label;
}

bool Column::labelsSyncIntsMap(const intstrmap &labelPerKey)
{
	JASPTIMER_SCOPE(Column::labelsSyncInts);
	
	beginBatchedLabelsDB();

	std::set<int> keys;
	for (const auto &keyLabel : labelPerKey)
		keys.insert(keyLabel.first);

	bool changed = labelsSyncInts(keys);

	for (Label * label : _labels)
	{
		int					key					= label->value();
		const std::string &	new_string_label	= labelPerKey.at(key),
							old_string_label	= label->label();

		if (new_string_label != old_string_label)
		{
			label->setLabel(new_string_label);
			changed = true;
		}
	}

	endBatchedLabelsDB();

	return changed;
}

bool Column::labelsSyncInts(const std::set<int> &uniqueValues)
{
	JASPTIMER_SCOPE(Column::labelsSyncInts);
	
	bool wasBatched = _batchedLabel; //we can be called from labelsSyncIntsMap
	if(!wasBatched)
		beginBatchedLabelsDB();

	intintmap	modifiedValues;
	intset		valuesToAdd		= uniqueValues,
				valuesToRemove;
	bool		isChanged		= false;

	for (Label * label : _labels)
	{
		if(label->originalValue().type() != Json::intValue)
		{
			isChanged = true;
			if(label->label() == "") //if missing label set it now
				label->setLabel(label->originalValueAsString());

			// this behaviour was there in the original Labels::syncInts(const std::set<int> &values) function.
			// Not sure if this is what we want though. (this would be the equivalent of setting _hasInt = true)
			label->setOriginalValue(label->value());
		}

		if(uniqueValues.count(label->value()))	valuesToAdd.erase(label->value());
		else									valuesToRemove.insert(label->value());
	}

	labelsRemoveValues(valuesToRemove);

	for (int value : valuesToAdd)
		labelsAdd(value);


	if(!wasBatched)
		endBatchedLabelsDB();

	return isChanged || (valuesToAdd.size() + valuesToRemove.size() > 0);
}


//This function could be simplified or at least made a bit more readable...
strintmap Column::labelsSyncStrings(const stringvec &new_values, const strstrmap &new_labels, bool *changedSomething)
{
	JASPTIMER_SCOPE(Column::labelsSyncStrings);
	
	beginBatchedLabelsDB();

	//Prepare a list of values we might have to add, labeltext -> labelkey, if they aren't there yet.
	stringset	valuesToAdd;
	intset		valuesToRemove;
	strintmap	result;

	for (const std::string& v : new_values)
		valuesToAdd.insert(v);

	//Check whether the entries in mapValuesToAdd are already there, or using originalValue because the datafile doesnt know what the user did
	for (const Label * label : _labels)
	{
		std::string labelText	= label->originalValueAsString();
		int labelValue			= label->value();

		auto it = valuesToAdd.find(labelText);
		if (it != valuesToAdd.end())
		{
			result[labelText] = labelValue;
			valuesToAdd.erase(labelText);
		}
		else
			valuesToRemove.insert(labelValue); //It it no longer present in the datafile
	}

	if(changedSomething != nullptr && (valuesToRemove.size() > 0 || valuesToAdd.size() > 0))
		*changedSomething = true;

	if (valuesToRemove.size() > 0)
	{
		labelsRemoveValues(valuesToRemove);
		//result = labelsResetValues(maxLabelKey);
	}

	for (const std::string & newLabel : valuesToAdd)
		result[newLabel] = labelsAdd(newLabel);

	//Now all thats left is to use the new_labels mapping. As far as I can see during this refactor it is only used when importing from ReadStat
	//And there we can have string-value and string-label in the same datafile that can be different.
	//Perhaps the label changed there, if so overwrite it so the user gets to see it.
	for (Label * label : _labels)
	{
		std::string origLabel = label->originalValueAsString();

		if(new_labels.count(origLabel) && new_labels.at(origLabel) != label->label())
		{
				label->setLabel(new_labels.at(origLabel));

				if(changedSomething != nullptr)
					*changedSomething = true;
		}
	}

	endBatchedLabelsDB();

	return result;
}

bool Column::isValueEqual(size_t row, double value) const
{
	JASPTIMER_SCOPE(Column::isValueEqual);

	if (row >= rowCount())
		return false;

	if (_type != columnType::scale)
		return false;

	//isEmptyVal handles nan
	if (isEmptyValue(value))
		return isEmptyValue(_dbls[row]);
	else
		return _dbls[row] == value;

}

bool Column::isValueEqual(size_t row, int value) const
{
	JASPTIMER_SCOPE(Column::isValueEqual);

	if (row >= rowCount())
		return false;

	if (_type == columnType::scale)
		return _dbls[row] == value;

	int intValue = _ints[row];

	if (_type == columnType::nominal || _type == columnType::ordinal)
		return (intValue == value);

	//So we are in a nominalText column
	if (intValue == std::numeric_limits<int>::lowest())
		return value == std::numeric_limits<int>::lowest();

	if (row < _labels.size())
	{
		Label * lbl = labelByValue(intValue);

		if (lbl->originalValue().type() == Json::intValue)
			return lbl->originalValue().asInt() == value;
	}

	return false;
}

bool Column::isValueEqual(size_t row, const std::string &value) const
{
	JASPTIMER_SCOPE(Column::isValueEqual);

	if (row >= rowCount())
		return false;

	switch (_type)
	{
		case columnType::scale:		return std::to_string(_dbls[row]) == value;
		case columnType::nominal:
		case columnType::ordinal:	return std::to_string(_ints[row]) == value;
		default:
			return	_ints[row] == std::numeric_limits<int>::lowest()
					? isEmptyValue(value)
					: value == getValue(row);
	}

	return false;
}

std::string Column::doubleToDisplayString(double dbl, bool fancyEmptyValue) const
{
	if (dbl > std::numeric_limits<double>::max())				return "∞";
	else if (dbl < std::numeric_limits<double>::lowest())		return "-∞";
	else if (isEmptyValue(dbl))									return fancyEmptyValue ? ColumnUtils::emptyValue : "";
	else														return ColumnUtils::doubleToString(dbl);
}

std::string Column::getValue(size_t row, bool fancyEmptyValue) const
{
	if (row < rowCount())
	{
		if (_type == columnType::scale)
			return doubleToDisplayString(_dbls[row], fancyEmptyValue);

		else if (_ints[row] != std::numeric_limits<int>::lowest())
		{
			Label * label = labelByValue(_ints[row]);

			if(label)
				return label->originalValueAsString();
		}
	}

	return fancyEmptyValue ? ColumnUtils::emptyValue : "";
}

Label * Column::labelByRow(int row) const
{
	if (row < rowCount() && _type != columnType::scale && _ints[row] != std::numeric_limits<int>::lowest())
			return labelByValue(_ints[row]);

	return nullptr;
}

bool Column::convertValueToIntForImport(const std::string &strValue, int &intValue) const
{
	JASPTIMER_SCOPE(Column::convertValueToIntForImport);

	if(isEmptyValue(strValue))
	{
			intValue = std::numeric_limits<int>::lowest();
			return true;
	}

	return ColumnUtils::getIntValue(strValue, intValue);
}

bool Column::convertValueToDoubleForImport(const std::string & strValue, double & doubleValue) const
{
	std::string v = strValue;
	ColumnUtils::deEuropeaniseForImport(v);

	if(isEmptyValue(v))
	{
			doubleValue = NAN;
			return true;
	}

	return ColumnUtils::getDoubleValue(strValue, doubleValue);
}


bool Column::setStringValueToRowIfItFits(size_t row, const std::string & value, bool & changed, bool & typeChanged)
{
    JASPTIMER_SCOPE(Column::setStringValueToRowIfItFits);
    
	typeChanged = changed = false;

	bool convertedSuccesfully = value == "";

	if(convertedSuccesfully && _type != columnType::nominalText)
	{
		if(_type == columnType::scale ? setValue(row, NAN) : setValue(row, std::numeric_limits<int>::lowest()))
			changed = true;
	}
	else
		switch(_type)
		{
		case columnType::scale:
		{
			double newDoubleToSet = NAN;

			if(convertValueToDoubleForImport(value, newDoubleToSet))
			{
				if(setValue(row, newDoubleToSet))
					changed = true;

				convertedSuccesfully = true;
			}
			break;
		}

		case columnType::nominalText:
		case columnType::nominal:
		case columnType::ordinal:
		{
			convertedSuccesfully = true;

			Label * newLabel		= labelByDisplay(value);
			Label * oldLabel		= labelByValue(_ints[row]);

			int	newIntegerToSet = -1;

			if (newLabel)
				newIntegerToSet = newLabel->value();
			else
			{
				if (_type == columnType::nominalText)
					// Be careful: labelsAdd works differently with a string or an integer
					newIntegerToSet = labelsAdd(value);
				else
				{
					int intValue = -1;
					if (ColumnUtils::getIntValue(value, intValue))
					{
						// As the nominal/ordinal column has string labels, the user may enter either a label or direclty the integer value.
						// So we need to check this also.
						newLabel = labelByValue(intValue);
						if (newLabel)
							newIntegerToSet = newLabel->value();
						else
							newIntegerToSet = labelsAdd(intValue);
					}
					else
					{
						// A string has been set to a nominal/ordinal column, and this string is not a label of this column
						// By setting convertedSuccesfully to false, this will convert this column to nominalText
						convertedSuccesfully = false;
					}
				}
			}


			if(convertedSuccesfully && newIntegerToSet != _ints[row])
			{
				int refs = 0;
				for(int i : _ints)
					if(_ints[row] == i)
						refs++;

				if(setValue(row, newIntegerToSet))
					changed = true;

				if(refs == 1) //only this row has this label, so we'll remove it
				{
					for(size_t i=0; i<_labels.size(); i++)
						if(_labels[i] == oldLabel)
						{
							_labels.erase(_labels.begin() + i);
							oldLabel->dbDelete();
							break;
						}

					//Make sure users dont change their columns from scalar to nominal text all the time by typing a letter by accident somewhere, so try to convert back if it seems reasonable to do so
					if(_type != _preEditType && _preEditType == columnType::scale)
					{
						double dbl;
                        if(     (
                                    !oldLabel ||
									!convertValueToDoubleForImport(oldLabel->label(),	dbl)
                                )
								&&
								convertValueToDoubleForImport(value,				dbl))
							typeChanged = changeType(_preEditType) == columnTypeChangeResult::changed; //Just try it
					}
					else if(_type == columnType::nominalText && (_preEditType == columnType::nominal || _preEditType == columnType::ordinal))
					{
						bool tryValueIntegers = false;
						int integer;
						if(     (
									!oldLabel ||
									!convertValueToIntForImport(oldLabel->label(),	integer)
								)
								&&
								convertValueToIntForImport(value,				integer))
						{
							typeChanged = changeType(_preEditType) == columnTypeChangeResult::changed; //Just try it
							tryValueIntegers = !typeChanged;
						}
						else
							tryValueIntegers = true;

						if(tryValueIntegers)
						{
							//maybe there are non-integer labels yet integer values?
							bool onlyIntegers = true;
							int integer;
							for(size_t i=0; onlyIntegers && i<_labels.size(); i++)
								if  ( ! (	_labels[i]->originalValue().isInt() ||
											(_labels[i]->originalValue().isDouble() && double(int(_labels[i]->originalValue().asDouble())) == _labels[i]->originalValue().asDouble())
										)
									)
									onlyIntegers = false;

							if (onlyIntegers)
								_changeColumnToNominalOrOrdinal(_preEditType);
						}
					}

					delete oldLabel;

					_resetLabelValueMap();
					_dbUpdateLabelOrder();
				}
			}

			break;
		}

		default:
			convertedSuccesfully = false;
			break;
		}

	return convertedSuccesfully;
}

bool Column::setValue(size_t row, int value, bool writeToDB)
{
	JASPTIMER_SCOPE(Column::setValue int);

	if(row >= _ints.size())
		return false;
	
	bool changed = _ints[row] != value;

	_ints[row] = value;
	
	if(writeToDB)
	{
		db().columnSetValue(_id, row, value);
		incRevision();
	}

	return changed;
}

bool Column::setValue(size_t row, double value, bool writeToDB)
{
	JASPTIMER_SCOPE(Column::setValue double);

	if(row >= _dbls.size())
		return false;
	
	bool changed = !Utils::isEqual(_dbls[row], value);

	_dbls[row] = value;

	if(writeToDB)
	{
		db().columnSetValue(_id, row, value);
		incRevision();
	}

	return changed;
}

void Column::setValues(const intvec & values)
{
	_ints = values;

	if(!_data->writeBatchedToDB())
		db().columnSetValues(_id, _ints);
}

void Column::setValues(const doublevec & values)
{
	_dbls = values;

	if(!_data->writeBatchedToDB())
		db().columnSetValues(_id, _dbls);
}

void Column::rowInsertEmptyVal(size_t row)
{
	if(type() == columnType::scale)	_dbls.insert(_dbls.begin() + row, NAN);
	else							_ints.insert(_ints.begin() + row, std::numeric_limits<int>::lowest());
}

void Column::rowDelete(size_t row)
{
	if(type() == columnType::scale)	_dbls.erase(_dbls.begin() + row);
	else							_ints.erase(_ints.begin() + row);
}

void Column::setRowCount(size_t rows)
{
	if(type() == columnType::scale)		{ _dbls.resize(rows); _ints.resize(0); }
	else								{ _ints.resize(rows); _dbls.resize(0); }
}

Label *Column::labelByValue(int value) const
{
	JASPTIMER_SCOPE(Column::labelByValue);

	return _labelByValueMap.count(value) ? _labelByValueMap.at(value) : nullptr;
}

Label *Column::labelByDisplay(const std::string & display) const
{
	JASPTIMER_SCOPE(Column::labelByDisplay);

	for(Label * label : _labels)
		if(label->label() == display)
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

std::set<size_t> Column::labelsMoveRows(std::vector<size_t> rows, bool up)
{
	std::vector<Label*> new_labels(_labels.begin(), _labels.end());

	int mod = up ? -1 : 1;

	std::sort(rows.begin(), rows.end(), [&](const size_t & l, const size_t & r) { return up ? l < r : r < l; });

	for (size_t row : rows)
		if(int(row) + mod < 0 || int(row) + mod >= int(_labels.size()))
			return {}; //Because we can't move *out* of our _labels for obvious reasons

	std::set<size_t> rowsChanged;

	for (size_t row : rows)
	{
		std::iter_swap(new_labels.begin() + row, new_labels.begin() + (row + mod));
		rowsChanged.insert(row);
		rowsChanged.insert(row + mod);
	}

	_labels = new_labels;

	_dbUpdateLabelOrder();

	return rowsChanged;
}

void Column::labelsReverse()
{
	std::reverse(_labels.begin(), _labels.end());

	_dbUpdateLabelOrder();
}


std::string Column::operator[](size_t row)
{
	std::string result = ColumnUtils::emptyValue;

	if (row < rowCount())
	{
		if (_type == columnType::scale)
			return doubleToDisplayString(_dbls[row], true);
		else
			return _getLabelDisplayStringByValue(_ints[row]);
	}

	return result;
}

std::string Column::_getLabelDisplayStringByValue(int key) const
{
	if (key == std::numeric_limits<int>::lowest())
		return ColumnUtils::emptyValue;

	if(_labelByValueMap.count(key))
		return _labelByValueMap.at(key)->label(true);

	return std::to_string(key);
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
	return type() != columnType::scale && !allLabelsPassFilter();
}


void Column::resetFilter()
{
	db().transactionWriteBegin();
	
	for(Label * label : _labels)
		label->setFilterAllows(true);
	
	incRevision();

	db().transactionWriteEnd();
}

void Column::incRevision()
{
	assert(_id != -1);

	if(!_data->writeBatchedToDB())
	{
		_revision = db().columnIncRevision(_id);
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
	JASPTIMER_SCOPE(Column::isColumnDifferentFromStringValues);

	if(strVals.size() != rowCount()) return true;

	for(int row = 0; row < strVals.size(); row++)
		switch(type())
		{
		case columnType::ordinal:
		case columnType::nominal:
		{
			int intValue;
			if(!convertValueToIntForImport(strVals[row], intValue) || !isValueEqual(row, intValue))
				return true;
			break;
		}

		case columnType::scale:
		{
			double doubleValue;
			if(!convertValueToDoubleForImport(	strVals[row], doubleValue) || !isValueEqual(row, doubleValue))
				return true;
			break;
		}

		case columnType::nominalText:
		{
			if(!isValueEqual(row, strVals[row]))
				return true;
			break;
		}

		default:
			return true;
		}

	return false;
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
	json["isComputed"]		= _isComputed;
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

	if (_data->hasCustomEmptyValues(_name))
	{
		Json::Value jsonCustomEmptyValues(Json::arrayValue);
		for (const std::string& val : _data->emptyValues(_name))
			jsonCustomEmptyValues.append(val);
		json["customEmptyValues"]	= jsonCustomEmptyValues;
	}

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
	_isComputed			= json["isComputed"].asBool();
	_analysisId			= json["analysisId"].asInt();

	db().columnSetComputedInfo(_id, _analysisId, _invalidated, _codeType, _rCode, _error, constructorJsonStr());


	_dbls.clear();
	for (const Json::Value& dblJson : json["dbls"])
		_dbls.push_back(dblJson.asDouble());

	db().columnSetValues(_id, _dbls);

	_ints.clear();
	for (const Json::Value& dblJson : json["ints"])
		_ints.push_back(dblJson.asInt());

	db().columnSetValues(_id, _ints);

	for (Label* label : _labels)
	{
		_labelByValueMap.erase(label->value());
		label->dbDelete();
		delete label;
	}
	_labelByValueMap.clear();
	_labels.clear();

	const Json::Value& labels = json["labels"];
	if (labels.isArray())
	{
		for (const Json::Value& labelJson : labels)
			labelsAdd(labelJson["value"].asInt(), labelJson["label"].asString(), labelJson["filterAllows"].asBool(), labelJson["description"].asString(), labelJson["originalValue"].asString(), labelJson["order"].asInt(), -1);
	}

	if (json.isMember("customEmptyValues"))
	{
		stringset emptyValues;
		const Json::Value& jsonCustomEmptyValues = json["customEmptyValues"];
		if (jsonCustomEmptyValues.isArray())
		{
			for (const Json::Value& emptyVal : jsonCustomEmptyValues)
				emptyValues.insert(emptyVal.asString());
		}
		_data->setCustomEmptyValues(_name, emptyValues);
	}
	else
		_data->setHasCustomEmptyValues(_name, false);


	incRevision();
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
	return ColumnUtils::isEmptyValue(val, emptyValues());
}

bool Column::isEmptyValue(const double val) const
{
	return ColumnUtils::isEmptyValue(val, doubleEmptyValues());
}

