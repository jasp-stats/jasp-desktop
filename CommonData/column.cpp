#include "log.h"
#include "column.h"
#include "timers.h"
#include "dataset.h"
#include "columnutils.h"
#include "databaseinterface.h"

bool Column::_autoSortByValuesByDefault = true;

bool Column::autoSortByValuesByDefault()
{
	return _autoSortByValuesByDefault;	
}

void Column::setAutoSortByValuesByDefault(bool autoSort)
{
	_autoSortByValuesByDefault = autoSort;
}

Column::Column(DataSet * data, int id)
:	DataSetBaseNode(dataSetBaseNodeType::column, data->dataNode()),
	_data(				data),
	_id(				id),
	_emptyValues(		new EmptyValues(data->emptyValues())),
	_doubleDummy(		new Label(this)),
	_autoSortByValue(	_autoSortByValuesByDefault)
{
	if(_id != -1)
		db().columnSetAutoSort(_id, _autoSortByValue); //Store autosort in db
}

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
	
	db().columnGetBasicInfo(	_id, _name, _title, _description, _type, _revision, emptyVals, _autoSortByValue);
	db().columnGetComputedInfo(	_id, _analysisId, _invalidated, _codeType, _rCode, _error, _constructorJson);
	
	_emptyValues->fromJson(emptyVals);

	_resetLabelValueMap();
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

	labelsClear(false);
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
		false,
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

bool Column::setName(const std::string &name)
{
	JASPTIMER_SCOPE(Column::setName);

	if(_name == name || name.empty())
		return false;

	std::string orgName = _name;
	_name = getUniqueName(name);

	if(_title.empty() || _title == orgName)
		setTitle(_name);

	db().columnSetName(_id, _name);
	incRevision();

	return true;
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
	
	incRevision(false);
}

bool Column::setCustomEmptyValues(const stringset& customEmptyValues)
{
	JASPTIMER_SCOPE(Column::setCustomEmptyValues);
	
	if (_emptyValues->emptyStrings() == customEmptyValues)
		return false;

	_emptyValues->setEmptyValues(customEmptyValues, _emptyValues->hasEmptyValues());
	db().columnSetEmptyVals(_id, _emptyValues->toJson().toStyledString());
	
	incRevision(false);
	
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

void Column::setAutoSortByValue(bool sort)
{
	JASPTIMER_SCOPE(Column::setAutoSortByValue);

	if(sort == _autoSortByValue)
		return;
	
	_autoSortByValue = sort;
	
	db().columnSetAutoSort(_id, _autoSortByValue);
	
	labelsHandleAutoSort();
	
	return;	
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

void Column::setIndex(int index)
{
	JASPTIMER_SCOPE(Column::setIndex);

	db().columnSetIndex(id(), index);
	incRevision();
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


void Column::setCompColStuff(bool invalidated, bool forceSourceColType, computedColumnType codeType, const std::string &rCode, const std::string &error, const Json::Value &constructorJson)
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

	if(columnType != columnType::unknown)
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

columnType Column::setValues(const stringvec & values, const stringvec & labels, int thresholdScale, bool * aChange)
{
	JASPTIMER_SCOPE(Column::setValues);

	if(aChange && _dbls.size() != values.size())
		(*aChange) = true;

	assert(values.size() == labels.size() || labels.size() == 0);

	size_t prevSize = _ints.size();
	
	_dbls.resize(values.size());
	_ints.resize(values.size());
	
	for(size_t resetRow=prevSize; resetRow<_ints.size(); resetRow++)
	{
		_ints[resetRow]	= EmptyValues::missingValueInteger;
		_dbls[resetRow] = EmptyValues::missingValueDouble;
	}
	
	bool	onlyDoubles = true, 
			onlyInts	= true;
	
	//Weve already made sure we have only 1 label per value and display combo, because otherwise this will get too complicated

	intset	ints;  // to suggest whether this is a scalar or not we need to know whether we have more than treshold ints or not.
	int		tmpInt;
	double	tmpDbl;
	
	for(size_t i=0; i<values.size(); i++)
	{
		if(setValue(i, values[i], labels.size() ? labels[i] : "", false) && aChange)
			(*aChange) = true;
		
		
		if(values[i] != "" || (labels.size() && labels[i] != ""))
		{
			if(ColumnUtils::getIntValue(values[i], tmpInt))
				ints.insert(tmpInt);
			else
				onlyInts = false;
			
			if(!ColumnUtils::getDoubleValue(values[i], tmpDbl))
				onlyDoubles = false;
		}
	}
	
	if(labelsRemoveOrphans() && aChange)
		(*aChange) = true;
	
	
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
		else if(!isEmptyValue(_dbls[i]))
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
	{
		if(data.size() == _data->filter()->filteredRowCount())
		{
			const boolvec & filtered = _data->filter()->filtered();
			stringvec		newData;
							newData	 . reserve(filtered.size());
			
			for(size_t iFilter=0, iData=0; iFilter < filtered.size() && iData < data.size(); iFilter++)
				newData.push_back(filtered[iFilter] ? data[iData++] : "");
				
			data = newData;
		}
		else
			data.resize(_data->rowCount());
	}

	bool			changes		= _type != colType,
					toScale		= colType == columnType::scale;
	stringvec		otherData	= data,
				&	values		= toScale  ? data : otherData,
				&	labels		= !toScale ? data : otherData;
	
	// If we are going to allow users to edit things it would be nice if we didnt just throw it away so rough
	// See: https://github.com/jasp-stats/INTERNAL-jasp/issues/2680
	// All we have to do is find the value/label per label/value given depending on the selected columnType
	
	
	strstrmap											replacePerKey;
	std::function<std::string(const std::string &)>		getOther		= [&](const std::string & in) 
	{ 
		if(!replacePerKey.count(in))
		{
			Label * tmp = toScale ? labelByValue(in) : labelByDisplay(in); 
			replacePerKey[in] = !tmp ? in : toScale ? tmp->label() : tmp->originalValueAsString();
		}
		
		return replacePerKey.at(in);
	};
	
	for(size_t i=0; i<data.size(); i++)
		otherData[i] = getOther(data[i]);
	
	setValues(values, labels, 0, &changes);
	setType(colType);
	labelsTempReset();
	
	labelsHandleAutoSort();
	
	return changes;
}

void Column::_dbUpdateLabelOrder(bool noIncRevisionWhenBatchedPlease)
{
	JASPTIMER_SCOPE(Column::_dbUpdateLabelOrder);
	
	if(batchedLabelDepth())
	{
		if(!noIncRevisionWhenBatchedPlease)
			incRevision();
		return;
	}
	
	labelsHandleAutoSort(false);

	intintmap orderPerDbIds;

	_highestIntsId = 0;
	for(size_t i=0; i<_labels.size(); i++)
	{
		_labels[i]->setOrder(i);
		orderPerDbIds[_labels[i]->dbId()] = i;

		_highestIntsId = std::max(_highestIntsId, _labels[i]->intsId());
	}

	db().labelsSetOrder(orderPerDbIds);
	
	incRevision(false);
}

void Column::_sortLabelsByOrder()
{
	std::sort(_labels.begin(), _labels.end(), [](const Label * l, const Label * r) { return l->order() < r->order(); });
}

void Column::labelsClear(bool doIncRevision)
{
	db().labelsClear(_id);
	_labels.clear();
	_labelByIntsIdMap.clear();
	_labelByValDis.clear();
	_highestIntsId = 0;
	
	if(doIncRevision)
		incRevision(false);
}

void Column::beginBatchedLabelsDB()
{
	_batchedLabelDepth++;
}

void Column::endBatchedLabelsDB(bool wasWritingBatch)
{
	assert(_batchedLabelDepth > 0);
	_batchedLabelDepth--;
	
	for(size_t i=0; i<_labels.size(); i++)
		_labels[i]->setOrder(i);
	
	if(_batchedLabelDepth == 0)
	{
		if(wasWritingBatch)
		{
			db().labelsWrite(this);
			incRevision(); //Should trigger reload at engine end
		}
	}	
}

int Column::labelsAdd(int display)
{
	return labelsAdd(std::to_string(display));
}

int Column::labelsAdd(const std::string &display)
{

	JASPTIMER_SCOPE(Column::labelsAdd displaystring);

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
	JASPTIMER_SCOPE(Column::labelsAdd 3 args);

	return labelsAdd(++_highestIntsId, display, true, description, originalValue);
}

int Column::_labelMapIt(Label * label)
{
	_labelByIntsIdMap[label->intsId()]		= label;
	_labelByValDis[label->origValDisplay()]	= label;

	_highestIntsId = std::max(_highestIntsId, label->intsId());

	_dbUpdateLabelOrder(true);
	return label->intsId();
}

int Column::labelsAdd(int value, const std::string & display, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order, int id)
{
	JASPTIMER_SCOPE(Column::labelsAdd lotsa arg);

	auto valDisplay = std::make_pair(Label::originalValueAsString(this, originalValue), display);

	if(_labelByValDis.count(valDisplay))
		return _labelByValDis.at(valDisplay)->intsId();

	Label * label = new Label(this, display, value, filterAllows, description, originalValue, order, id);
	_labels.push_back(label);
	
	return _labelMapIt(label);
}

int Column::labelsSet(int labelIndex, int value, const std::string &display, bool filterAllows, const std::string &description, const Json::Value &originalValue, int order, int id)
{
	JASPTIMER_SCOPE(Column::labelsSet);

	Label * label = nullptr;
	if(_labels.size() > labelIndex)
	{
		label = _labels[labelIndex];
		label->setInformation(this, id, order, display, value, filterAllows, description, originalValue);
	}
	else
	{
		if(_labels.size() != labelIndex)
			Log::log() << "Labels are not being set in a sensible order it seems." << std::endl;

		if(id == -1)
			Log::log() << "This functions expects a label to be set from the DB, so why is dbId -1?" << std::endl;

		if(_labels.size() < labelIndex+1)
			_labels			. resize(labelIndex+1);
		_labels[labelIndex]	= new Label(this, display, value, filterAllows, description, originalValue, order, id);
		label				= _labels[labelIndex];
	}

	assert(label);

	return _labelMapIt(label);
}

void Column::labelsRemoveByIntsId(std::set<int> valuesToRemove, bool updateOrder)
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
					
					auto valDis = std::make_pair(label->originalValueAsString(), label->labelDisplay());
					if(_labelByValDis.count(valDis) && _labelByValDis.at(valDis) == label)
						_labelByValDis.erase(valDis);
						
					label->dbDelete();
					delete label;
					return true;
				}
				return false;
			}),
			_labels.end());

	if(updateOrder)
		_dbUpdateLabelOrder();
}

strintmap Column::labelsResetValues(int & maxValue)
{
	JASPTIMER_SCOPE(Column::labelsResetValues);

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

	_highestIntsId = maxValue;

	endBatchedLabelsDB();

	return result;
}

void Column::labelsRemoveBeyond(size_t desiredLabelsSize)
{
	for(size_t i=desiredLabelsSize; i<_labels.size(); i++)
		delete _labels[i];
	
	if(desiredLabelsSize < _labels.size())
		_labels.resize(desiredLabelsSize);

	_resetLabelValueMap();
}

void Column::labelsTempReset()
{
	_labelsTemp					. clear();
	_labelsTempDbls				. clear();
	_labelsTempToIndex			. clear();
	_labelsTempRevision			= -1;
	_labelsTempMaxWidth			= 0;
	_labelsTempNumerics			= 0;
	_labelByNonEmptyIndex		.clear();
	_labelNonEmptyIndexByLabel	.clear();
	
	nonFilteredCountersReset();
}

int Column::labelsTempCount()
{
	if(_revision != _labelsTempRevision)
	{
		//first collect the labels that are actually Label
		labelsTempReset();
		_labelsTemp			. reserve(_labels.size());
		_labelsTempDbls		. reserve(_labels.size());
		
		size_t nonEmptyIndex = 0;
		for(size_t r=0; r<_labels.size(); r++)
			if(!_labels[r]->isEmptyValue())
			{
				_labelsTemp												. push_back(_labels[r]->label());
				_labelsTempDbls											. push_back(_labels[r]->originalValue().isDouble() ? _labels[r]->originalValue().asDouble() : EmptyValues::missingValueDouble);
				_labelsTempToIndex[_labelsTemp[_labelsTemp.size()-1]]	= _labelsTemp.size()-1; //We store the index in _labelsTemp in a map.
				_labelByNonEmptyIndex[nonEmptyIndex]					= _labels[r];
				_labelNonEmptyIndexByLabel[_labels[r]]					= nonEmptyIndex;
				
				if(!isEmptyValue(*_labelsTempDbls.rbegin()))
					_labelsTempNumerics++;
				
				nonEmptyIndex++;
			}
		
		doubleset dblset;
		
		for(size_t r=0; r<rowCount(); r++)
		{
			if(!isEmptyValue(_dbls[r]) && _ints[r] == Label::DOUBLE_LABEL_VALUE)
				dblset.insert(_dbls[r]);
		}
		
		doublevec dbls(dblset.begin(), dblset.end());
		
		std::sort(dbls.begin(), dbls.end());
		
		//There might also be "double" values that should also be shown in the editor so we go through everything and add them to _labelsTemp and _labelsTempToIndex	
		for(double dbl : dbls)
		{
			const std::string doubleLabel = doubleToDisplayString(dbl, false);
			
			if(!doubleLabel.empty() && !_labelsTempToIndex.count(doubleLabel))
			{
				_labelsTemp						. push_back(doubleLabel);
				_labelsTempDbls					. push_back(dbl);
				_labelsTempToIndex[doubleLabel] = _labelsTemp.size()-1;
				_labelsTempMaxWidth				= std::max(_labelsTempMaxWidth, qsizetype(_labelsTemp[_labelsTemp.size()-1].size()));
				_labelsTempNumerics				++;
			}
		}

		//Make sure we dont do this too often be remembering at which revision we created the temp values:
		_labelsTempRevision = _revision;
	}
	
	return _labelsTemp.size();
}

int Column::nonFilteredNumericsCount()
{
	if (_nonFilteredNumericsCount == -1)
	{
		doubleset numerics;

		for(size_t r=0; r<_data->rowCount(); r++)
			if(_data->filter()->filtered()[r] && !isEmptyValue(_dbls[r]))
					numerics.insert(_dbls[r]);

		_nonFilteredNumericsCount = numerics.size();
	}

	return _nonFilteredNumericsCount;
}

stringset Column::nonFilteredLevels()
{
	if (_nonFilteredLevels.empty())
	{
		for(size_t r=0; r<_data->rowCount(); r++)
			if(_data->filter()->filtered()[r])
			{
				if(_ints[r] != Label::DOUBLE_LABEL_VALUE)
				{
					Label * label = labelByIntsId(_ints[r]);
					if(label && !label->isEmptyValue())
						_nonFilteredLevels.insert(label->label());
				}
				else if(!isEmptyValue(_dbls[r]))
					_nonFilteredLevels.insert(ColumnUtils::doubleToString(_dbls[r]));
			}
	}

	return _nonFilteredLevels;
}

void Column::nonFilteredCountersReset()
{
	_nonFilteredLevels.clear();
	_nonFilteredNumericsCount = -1;
}

int Column::labelsTempNumerics()
{
	labelsTempCount(); //generate the list if need be
	
	return _labelsTempNumerics;
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

int Column::labelIndexNonEmpty(Label *label) const
{			
	return !_labelNonEmptyIndexByLabel.count(label) ? -1 : _labelNonEmptyIndexByLabel.at(label);
}

Label * Column::labelByIndexNotEmpty(int index) const
{
	return !_labelByNonEmptyIndex.count(index) ? nullptr : _labelByNonEmptyIndex.at(index);
}

size_t Column::labelCountNotEmpty() const
{
	size_t	nonEmpty = 0;
	
	for(size_t l=0; l<_labels.size(); l++)
		if(!_labels[l]->isEmptyValue())
			nonEmpty++;
	
	return nonEmpty;
}

std::string Column::labelsTempValue(size_t tempLabelIndex, bool fancyEmptyValue)
{
	if(labelsTempCount() <= tempLabelIndex)
		return "";
	
	Label * label = labelByIndexNotEmpty(tempLabelIndex);
	
	if(label)
		return label->originalValueAsString(fancyEmptyValue);
	
	//So its not from a Label, this means its from _dbls
	//So that means the display value is actually the same as the value so:
	return doubleToDisplayString(_labelsTempDbls[tempLabelIndex], fancyEmptyValue);
}

double Column::labelsTempValueDouble(size_t tempLabelIndex)
{
	if(labelsTempCount() <= tempLabelIndex)
		return EmptyValues::missingValueDouble;
	
	Label * label = labelByIndexNotEmpty(tempLabelIndex);
	
	if(label)
		return label->originalValue().isDouble() ? label->originalValue().asDouble() : EmptyValues::missingValueDouble;
	
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
	_labelByValDis.clear();

	for(Label * label : _labels)
	{
		_labelByIntsIdMap[label->intsId()]														= label;
		_labelByValDis[std::make_pair(label->originalValueAsString(), label->labelDisplay())]	= label;
	}
	
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

std::string Column::getValue(size_t row, bool fancyEmptyValue, bool ignoreEmptyValue, columnType asType) const
{
	if(asType == columnType::unknown)
		asType = _type;
	
	if (row < rowCount())
	{
		if (asType == columnType::scale || _ints[row] == Label::DOUBLE_LABEL_VALUE)
			return doubleToDisplayString(_dbls[row], fancyEmptyValue, ignoreEmptyValue);

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
	return _type == columnType::scale	
		?	getValue(row, fancyEmptyValue)
		:	getLabel(row, fancyEmptyValue);
}

std::string Column::getShadow(size_t row, bool fancyEmptyValue) const
{
	return _type != columnType::scale	
		?	getValue(row, fancyEmptyValue, true)
		:	getLabel(row, fancyEmptyValue, true);
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
	JASPTIMER_SCOPE(Column::valuesAsStrings);
	
	stringvec returnMe;
	returnMe.resize(_dbls.size());
	
	for(size_t i=0; i<returnMe.size(); i++)
		returnMe[i] = getValue(i);
	
	return returnMe;
}

stringvec Column::labelsAsStrings() const
{
	JASPTIMER_SCOPE(Column::labelsAsStrings);
	
	stringvec returnMe;
	returnMe.resize(_dbls.size());
	
	for(size_t i=0; i<returnMe.size(); i++)
		returnMe[i] = getLabel(i);
	
	return returnMe;
}

stringvec Column::displaysAsStrings() const
{
	JASPTIMER_SCOPE(Column::displaysAsStrings);
	
	stringvec returnMe;
	returnMe.resize(_dbls.size());
	
	for(size_t i=0; i<returnMe.size(); i++)
		returnMe[i] = getDisplay(i);
	
	return returnMe;
}

stringvec Column::dataAsRLevels(intvec & values, const boolvec & filter, bool useLabels )
{
	JASPTIMER_SCOPE(Column::dataAsRLevels);
	
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
	
	//make sure we have temp labels for any doubles/ints outside of labels
	labelsTempCount();
	size_t nonEmpty = 0;
		
	//First we try to find all levels, start with the known labels and then add any  doubles as labels.
	for(Label * label : _labels)
		if(!label->isEmptyValue())
		{
			_addLabel(useLabels ? label->labelDisplay() : label->originalValueAsString(false), false);
			nonEmpty++;
		}
	
	//Now we add the sorted temp dbl labels, so that we get the same order as shown in the variableswindow
	for(size_t lti=nonEmpty; lti<_labelsTempDbls.size(); lti++)
		_addLabel(doubleToDisplayString(_labelsTempDbls[lti], false), false);
	
	assert(filter.size() == rowCount() || filter.size() == 0);

	//We ignore emptyvalues and depending on whether filter is usable (length is data length) we filter out rows we dont need
	bool useFilter = filter.size() == rowCount();
	
	for(size_t row=0; row<rowCount(); row++)
		if(!useFilter || filter[row])
		{
			if(_ints[row] != Label::DOUBLE_LABEL_VALUE)
			{
				Label * label = labelByIntsId(_ints[row]);
				
				assert(label || _ints[row] == EmptyValues::missingValueInteger);
				
				if(label && !label->isEmptyValue())
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
				
				assert(label || _ints[row] == EmptyValues::missingValueInteger);
				
				if(label && !label->isEmptyValue())
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
	JASPTIMER_SCOPE(Column::dataAsRDoubles);

	
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
	
	std::sort(dbls.begin(), dbls.end());
	
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
	
	dbUpdateValues(false);
	
	return doubleLabelMap;
}

Label *Column::replaceDoublesTillLabelsRowWithLabels(size_t row, double returnForDbl)
{
	JASPTIMER_SCOPE(Column::replaceDoublesTillLabelsRowWithLabels);
	
	//row here is in the label editor, not in the data!	
	if(labelByIndexNotEmpty(row))
		return labelByIndexNotEmpty(row);
	
	doublevec	dbls;
	double		dbl;
	
	for(size_t r=labelCountNotEmpty()	;
		r<=row && r<_labelsTemp.size()	;
		r++						
	)
		if(ColumnUtils::getDoubleValue(_labelsTemp[r], dbl))
			dbls.push_back(dbl);
		else
			throw std::runtime_error("replaceDoublesTillLabelsRowWithLabels choked on a temp-label that cant be converted to double???"); //Should never ever occur because it starts from _labels.size!

	//the last dbl is the one we want so use it to get the right label from the map
	auto	labelPerDbl = replaceDoubleWithLabel(dbls);
	Label * label		= labelPerDbl[std::isnan(returnForDbl) || labelPerDbl.count(returnForDbl) == 0 ? dbl : returnForDbl];
	
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

void Column::labelValueChanged(Label *label, const Json::Value & previousOriginal)
{
	auto oldValDis	= std::make_pair(Label::originalValueAsString(this, previousOriginal), label->labelDisplay());
	bool merged		= _labelByValDis.count(label->origValDisplay()) != 0;
	
	if(merged)
		labelsMergeDuplicateInto(label);
	
	//Make sure it was registered before:
	assert(_labelByValDis[oldValDis] == label);
	//And that its new location is free:
	assert(_labelByValDis.count(label->origValDisplay()) == 0 || _labelByValDis.at(label->origValDisplay()) == label);

	double theDouble = label->originalValue().isDouble() ? label->originalValue().asDouble() : EmptyValues::missingValueDouble;
	
	for(size_t r=0; r<_dbls.size(); r++)
		if(_ints[r] == label->intsId())
			_dbls[r] = theDouble;

	_labelByValDis.erase(oldValDis);
	_labelByValDis[label->origValDisplay()] = label;

	if(merged)
		_dbUpdateLabelOrder();
	
	dbUpdateValues();
}

void Column::labelsHandleAutoSort(bool doDbUpdateEtc)
{
	if(_autoSortByValue)
		labelsOrderByValue(doDbUpdateEtc);	
}

void Column::labelDisplayChanged(Label *label, const std::string & previousDisplay)
{
	auto oldValDis = std::make_pair(label->originalValueAsString(), previousDisplay);
	bool merged		= _labelByValDis.count(label->origValDisplay()) != 0;
	
	if(merged)
		labelsMergeDuplicateInto(label);
	

	//Make sure it was registered before:
	assert(_labelByValDis[oldValDis] == label);
	//And that its new location is free:
	assert(_labelByValDis.count(label->origValDisplay()) == 0 || _labelByValDis.at(label->origValDisplay()) == label);

	_labelByValDis.erase(oldValDis);
	_labelByValDis[label->origValDisplay()] = label;
	
	if(merged)
		_dbUpdateLabelOrder();

	if(_labelsTempRevision < _revision)
		return; //We dont care about this change anymore if the list is out of date

	size_t labelIdx = labelIndexNonEmpty(label);
	
	if(_labelsTemp.size() > labelIdx)
		_labelsTemp[labelIdx] = label->label();
	
	//So we know that label is about to trigger an incRevision for the column through dbUpdate and checkForChanges
	_labelsTempRevision++;
}

void Column::labelValDisplayChanged(Label *label, const std::string &previousDisplay, const Json::Value &previousOriginal)
{
	auto	oldValDis	= std::make_pair(Label::originalValueAsString(this, previousOriginal), previousDisplay),
			newValDis	= std::make_pair(label->originalValueAsString(), label->labelDisplay());
	bool	merged		= _labelByValDis.count(label->origValDisplay()) != 0;
	
	if(merged)
		labelsMergeDuplicateInto(label);
	
	//Make sure it was registered before:
	assert(_labelByValDis[oldValDis] == label);
	//And that its new location is free:
	assert(_labelByValDis.count(label->origValDisplay()) == 0 || _labelByValDis.at(label->origValDisplay()) == label);

	double newOrigValDbl = label->originalValue().isDouble() ? label->originalValue().asDouble() : EmptyValues::missingValueDouble;
	
	//Lets assume that all occurences of a label in _dbls are the same.
	//So when we encounter one that is the same as what is passed here we can return immediately
	for(size_t r=0; r<_dbls.size(); r++)
		if(_ints[r] == label->intsId())
			_dbls[r] = newOrigValDbl;
	
	_labelByValDis.erase(oldValDis);
	_labelByValDis[label->origValDisplay()] = label;

	if(merged)
		_dbUpdateLabelOrder();
	
	dbUpdateValues();
}

Label * Column::labelByRow(int row) const
{
	if (row < rowCount() && _type != columnType::scale && _ints[row] != EmptyValues::missingValueInteger)
		return labelByIntsId(_ints[row]);

	return nullptr;
}

bool Column::setStringValue(size_t row, const std::string & userEntered, const std::string & labelButOnlyFromSpreadsheetPaste, bool writeToDB)
{
    JASPTIMER_SCOPE(Column::setStringValue);
    
	if(userEntered == "" && labelButOnlyFromSpreadsheetPaste == "")
	{
		const auto	cur = type() == columnType::scale ?  getValue(row, false, false) : getLabel(row, false, false);
		Label	*	label = labelByRow(row);
		
		if(label && label->isEmptyValue())	return false;
		if (cur == "")						return false;
		else								return setValue(row, EmptyValues::missingValueDouble, writeToDB);
	}
	
	double		newDoubleToSet	= EmptyValues::missingValueDouble;
	bool		itsADouble		= ColumnUtils::getDoubleValue(userEntered, newDoubleToSet),
				itsMissingVal	= isEmptyValue(userEntered),
				nothingThereYet	=	!std::any_of(_ints.begin(), _ints.end(), [&](int i)		{ return !(i == Label::DOUBLE_LABEL_VALUE || i == EmptyValues::missingValueInteger || labelByIntsId(i)->isEmptyValue()); }) 
								&&	!std::any_of(_dbls.begin(), _dbls.end(), [&](double d)	{ return !(std::isnan(d) || isEmptyValue(d)); });
	
	if(nothingThereYet && !itsMissingVal)
	{
		if(itsADouble)
		{
			if(!isEmptyValue(newDoubleToSet))
				setType(columnType::scale);
		}
		else if(!isEmptyValue(userEntered))
			setType(columnType::nominal);
	}
		
	return setValue(row, userEntered, labelButOnlyFromSpreadsheetPaste, writeToDB);
}

bool Column::setValue(size_t row, const std::string & value, const std::string & label, bool writeToDB)
{
	JASPTIMER_SCOPE(Column::setValue(size_t row, const std::string & value, const std::string & label, writeToDB));
    
	//If value != "" and label == "" that means we got copy pasted stuff in the viewer. And we just dont have labels, but we can treat it like we are editing
	//if both are "" we just want to clear the cell
	//the assumption is that this is not direct user-input, but internal jasp stuff.
	if(value == "" && label == "")
		return setValue(row, EmptyValues::missingValueDouble, writeToDB);
	
	bool	labelIsValue	= value == label,
			justAValue		= label == "";			///< To help us handle updates from synchronisation from csv (users might have added different label-texts
	double	newDoubleToSet	= EmptyValues::missingValueDouble,
			oldDouble		= _dbls[row];	
	bool	itsADouble		= ColumnUtils::getDoubleValue(value, newDoubleToSet);
	Label * newLabel		= justAValue ? labelByValue(value) : labelByValueAndDisplay(value, label);
	Label * oldLabel		= _ints[row] == Label::DOUBLE_LABEL_VALUE ? nullptr : labelByIntsId(_ints[row]);
	
	if(justAValue && !newLabel && itsADouble)
	{
		const std::string valueDbl = ColumnUtils::doubleToString(newDoubleToSet);
		newLabel = labelByValue(valueDbl);
		newLabel = newLabel ? newLabel : labelByValueAndDisplay(valueDbl, valueDbl);
	}
	
	if(justAValue && !newLabel)
		newLabel = labelByValueAndDisplay(value, value);
	
	if(!newLabel && (!justAValue && !labelIsValue)) //no new label found but value and label are different. Given that this exact combination does not occur we add a new label
		newLabel = labelByIntsId( labelsAdd(label, "", itsADouble ? Json::Value(newDoubleToSet) : value));
		
	if(!oldLabel && !newLabel && itsADouble) //no labels and it is a double, easy peasy
		return setValue(row, newDoubleToSet, writeToDB);

	if(newLabel)
	{
		if(newLabel->originalValue().isDouble())
			newDoubleToSet = newLabel->originalValue().asDouble();
		else 
			ColumnUtils::getDoubleValue(value, newDoubleToSet);
		
		return setValue(row, newLabel->intsId(), newDoubleToSet, writeToDB);
	}
		
	if(itsADouble && (labelIsValue || justAValue))
	{
		//There is no new label, an oldLabel AND we have a non-empty double in _dbls
		//This should mean that the label has this old double as a original value!
		if(	oldLabel
			&& !std::isnan(oldDouble)
			&& (	!oldLabel->originalValue().isDouble()
				|| !Utils::isEqual(_dbls[row], oldLabel->originalValue().asDouble())
				))
			Log::log() << "bool Column::setValue(" << row << ", '" << value << "', '" << label << "') had differences between _dbls[" << row << "](" << _dbls[row] << ") and oldLabel originalValue: '" << oldLabel->originalValueAsString() << "'" << std::endl;
		
		return setValue(row, newDoubleToSet, writeToDB);
	}
	else
		//there is no new label yet for this and so lets make one
		return setValue(row, labelsAdd(justAValue ? value : label, "", itsADouble ? Json::Value(newDoubleToSet) : value), writeToDB);
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
	
	if(writeToDB && !_data->writeBatchedToDB())
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
	
	labelsTempReset();
}

void Column::setRowCount(size_t rows)
{
	_dbls.resize(rows);
	_ints.resize(rows);
	
	labelsTempReset();
}

Label *Column::labelByIntsId(int value) const
{
	JASPTIMER_SCOPE(Column::labelByValue);

	return value != EmptyValues::missingValueInteger && _labelByIntsIdMap.count(value) ? _labelByIntsIdMap.at(value) : nullptr;
}

Label * Column::labelByDisplay(const std::string & display) const
{
	Labelset labels		= labelsByDisplay(display);
	return labels.size() == 0 ? nullptr : *labels.begin();	
}

Labelset Column::labelsByDisplay(const std::string & display) const
{
	JASPTIMER_SCOPE(Column::labelByDisplay);

	Labels found;
	std::copy_if(_labels.begin(), _labels.end(), std::back_inserter(found), [&display](Label * label)
	{
		return label->label() == display;
	});
	
	return Labelset(found.begin(), found.end());
}

Label * Column::labelByValue(const std::string & value) const
{
	Labelset labels		= labelsByValue(value);
	return labels.size() == 0 ? nullptr : *labels.begin();	
}

Labelset Column::labelsByValue(const std::string & value) const
{
	JASPTIMER_SCOPE(Column::labelByValue);

	Labels found;
	std::copy_if(_labels.begin(), _labels.end(), std::back_inserter(found), [&value](Label * label)
	{
		return label->originalValueAsString() == value;
	});
	
	return Labelset(found.begin(), found.end());
}

Label * Column::labelByValueAndDisplay(const std::string &value, const std::string &labelText) const
{
	JASPTIMER_SCOPE(Column::labelsByValueAndDisplay);

	auto valDis = std::make_pair(value, labelText);
	return _labelByValDis.count(valDis) == 0 ? nullptr : _labelByValDis.at(valDis);
}

void Column::labelsMergeDuplicateInto(Label * labelPrime)
{
	
	const std::string	value		= labelPrime->originalValueAsString(),
						labelText	= labelPrime->label();
	Labelset found;
	std::copy_if(_labels.begin(), _labels.end(), std::inserter(found, found.begin()), [&value, &labelText](Label * label)
	{
		return label->originalValueAsString() == value && label->label() == labelText;
	});
	
	if(found.size() > 1)
	{
		found.erase(labelPrime);
		
		for(Label * otherLabel : found)
			for(int & anInt : _ints)
				if(anInt == otherLabel->intsId())
					anInt = labelPrime->intsId();
		
		intset ids;
		
		for(Label * label : found)
			ids.insert(label->intsId());
		
		labelsRemoveByIntsId(ids, false);
		
		_labelByValDis[labelPrime->origValDisplay()] = labelPrime;
		labelsTempReset();
	}
}

bool Column::labelsRemoveOrphans()
{
	intset idsNotUsed;
	
	for(size_t labelIndex=0; labelIndex < _labels.size(); labelIndex++)
		idsNotUsed.insert(_labels[labelIndex]->intsId());
	
	for(int anInt : _ints)
		idsNotUsed.erase(anInt);
	
	labelsRemoveByIntsId(idsNotUsed);
	
	return idsNotUsed.size();
}

std::set<size_t> Column::labelsMoveRows(std::vector<qsizetype> rows, bool up)
{
	JASPTIMER_SCOPE(Column::labelsMoveRows);
	
	int mod = up ? -1 : 1;

	std::sort(rows.begin(), rows.end(), [&](const auto & l, const auto & r) { return up ? l < r : r < l; });
	
	replaceDoublesTillLabelsRowWithLabels(std::min(qsizetype(labelsTempCount()), rows.back() + 1));
	
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
	
	replaceDoublesTillLabelsRowWithLabels(labelsTempCount());
	std::reverse(_labels.begin(), _labels.end());
	
	labelsTempReset();
	_dbUpdateLabelOrder();
}

void Column::labelsOrderByValue(bool doDbUpdateEtc)
{
	JASPTIMER_SCOPE(Column::labelsOrderByValue);

	bool replaceAllDoubles = false;
	static double dummy;

	for(Label * label : labels())	
		if(!label->isEmptyValue())
		{
				replaceAllDoubles = true; // because if there is any label at all it will show up at the end after the doubles without a Label. If this label is a double and the same as the label we could try to turn it back into a non-Label. But it still would break for Labels with a double value and a non-double label. So instead lets just make a label for everything. We keep the doubles only if there are only doubles. This should speed up a lot of operations for massive double datasets so its worth the extra hassle I guess. Just like you, who just read to the end of this line that really really breaks the 80's guideline of 80 characterwide code ;)
				break;
		}

	if(replaceAllDoubles)
		replaceDoublesTillLabelsRowWithLabels(labelsTempCount());
	
	doublevec				asc			= valuesNumericOrdered();
	auto					alpha		= valuesAlphabeticalOffsets();
	size_t					ascMax		= asc.size()+1;
	std::map<double, int>	orderMap;
	
	for(size_t i=0; i<asc.size(); i++)
		orderMap[asc[i]] = i;
	
	//and now to write them back into the data
	for(Label * label : _labels)
	{
		double	aValue	= EmptyValues::missingValueDouble;
		
		if(label->originalValue().isDouble())
			aValue = label->originalValue().asDouble();
		else 
			ColumnUtils::getDoubleValue(label->originalValueAsString(), aValue);
		
		label->setOrder(!std::isnan(aValue) ? orderMap[aValue] : alpha[label] + ascMax);
	}
	
	_sortLabelsByOrder();
	labelsTempReset();
	if(doDbUpdateEtc)
		_dbUpdateLabelOrder();
}

doublevec Column::valuesNumericOrdered()
{
	doubleset	values;
	
	for(const Label * label : _labels)
	{
		double	aValue	= EmptyValues::missingValueDouble;
				
		if(label->originalValue().isDouble())
			aValue = label->originalValue().asDouble();
		else 
			ColumnUtils::getDoubleValue(label->originalValueAsString(), aValue);
		
		if(!std::isnan(aValue)) //not isEmptyValue because we want to use the output to rewrite the data again
			values.insert(aValue);
	}
	
	return doublevec(values.begin(), values.end());
}

std::map<Label*, size_t> Column::valuesAlphabeticalOffsets()
{
	std::map<Label*, size_t>	labelMap;
	Labels						alphaLabels;
	
	for(Label * label : _labels)
		if(!label->originalValue().isDouble())
			alphaLabels.push_back(label);
	
	std::sort(alphaLabels.begin(), alphaLabels.end(), [](const Label * l, const Label * r)
	{
		return l->originalValueAsString() < r->originalValueAsString();
	});
	
	for(size_t l=0; l<alphaLabels.size(); l++)
		labelMap[alphaLabels[l]] = l;
	
	return labelMap;
}

void Column::valuesReverse()
{
	JASPTIMER_SCOPE(Column::valuesReverse);
	
	replaceDoublesTillLabelsRowWithLabels(labelsTempCount());
	
	doublevec	asc = valuesNumericOrdered(),
				dsc	= asc;
	
	std::reverse(dsc.begin(), dsc.end());
	
	dbldblmap flipIt;
	
	for(size_t i=0; i<asc.size(); i++)
		flipIt[asc[i]] = dsc[i];
	
	//and now to write them back into the data
	for(Label * label : _labels)
	{
		double	aValue	= EmptyValues::missingValueDouble;
		
		if(label->originalValue().isDouble())
			aValue = label->originalValue().asDouble();
		else 
			ColumnUtils::getDoubleValue(label->originalValueAsString(), aValue);
		
		if(!std::isnan(aValue)) //not isEmptyValue because we want to use the output to rewrite the data again
			label->setOriginalValue(flipIt[aValue]);
	}
	
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

bool Column::isColumnDifferentFromStringValues(const std::string & title, const stringvec & strVals, const stringvec & strLabs, const stringset & strEmptyVals) const 
{
	return !(title == _title && strEmptyVals == emptyValues()->emptyStrings() && strVals == valuesAsStrings() && strLabs == labelsAsStrings());
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
	json["rCode"]			= _rCode;
	json["analysisId"]		= _analysisId;
	json["invalidated"]		= _invalidated;
	json["constructorJson"] = _constructorJson;
	json["autoSortByValue"] = _autoSortByValue;
	json["description"]		= _description;
	json["codeType"]		= int(_codeType);
	json["error"]			= _error;
	json["type"]			= int(_type);

	Json::Value jsonDbls(Json::arrayValue);
	for (double dbl : _dbls)
		jsonDbls.append(dbl);

	Json::Value jsonInts(Json::arrayValue);
	for (int i : _ints)
		jsonInts.append(i);

	json["customEmptyValues"]	= _emptyValues->toJson();

	json["labels"]				= serializeLabels();
	json["dbls"]				= jsonDbls;
	json["ints"]				= jsonInts;

	return json;
}

Json::Value	Column::serializeLabels() const
{
	Json::Value jsonLabels(Json::arrayValue);
	for (const Label* label : _labels)
		jsonLabels.append(label->serialize());
	
	return jsonLabels;
}

void Column::deserializeLabelsForCopy(const Json::Value & labels)
{
	labelsTempReset();

	beginBatchedLabelsDB();
	_labelByIntsIdMap.clear();
	_labelByValDis.clear();
	_labels.clear();

	if (labels.isArray())
		for (const Json::Value& labelJson : labels)
			labelsAdd(
						labelJson["intsId"]			.asInt(), 
						labelJson["label"]			.asString(), 
						labelJson["filterAllows"]	.asBool(), 
						labelJson["description"]	.asString(), 
						labelJson["originalValue"], 
						labelJson["order"]			.asInt(), 
						-1
			);
	
	endBatchedLabelsDB();
}

void Column::deserializeLabelsForRevert(const Json::Value & labels)
{
 	labelsTempReset();
	
	beginBatchedLabelsDB();
	
	//intset	updatedLbls;

	if (labels.isArray())
		for (const Json::Value& labelJson : labels)
		{
			int					intsId		= labelJson["intsId"]		.asInt(),
								order		= labelJson["order"]		.asInt();
			const std::string	labelStr	= labelJson["label"]		.asString(),
								description	= labelJson["description"]	.asString();
			bool				filterAllow = labelJson["filterAllows"]	.asBool();
			
			
			if(_labelByIntsIdMap.count(intsId))
			{
				Label * label = _labelByIntsIdMap[intsId];
				
				label->setOrder(			order						);
				label->setLabel(			labelStr					);
				label->setDescription(		description					);
				label->setFilterAllows(		filterAllow					);
				label->setOriginalValue(	labelJson["originalValue"]	);
				
				//updatedLbls.insert(intsId);
			}
			else
			{
				labelsAdd(intsId, labelStr, filterAllow, description, labelJson["originalValue"], order, -1);
							
				//updatedLbls.insert(intsId);
			}
		}
	
	
	_sortLabelsByOrder();
	
	/*intset missingLbls;
	for(auto & idLabel : _labelByIntsIdMap)
		if(!updatedLbls.count(idLabel.first))
			missingLbls.insert(idLabel.first);*/
	
	endBatchedLabelsDB();
	
	/* The following is already implied by endBatchedLabelsDB because it deletes all labels first anyway
	for(int id : missingLbls)
	{
		Label * deleteMe = 	_labelByIntsIdMap[id];
		
		for(size_t i=0;i<_labels.size(); i++)
			if(_labels[i] == deleteMe)
				_labels.erase(_labels.begin() + i);
		
		_labelByIntsIdMap.erase(id);
		deleteMe->dbDelete();
		delete deleteMe;	
	}*/
	
	incRevision();
}

void Column::deserialize(const Json::Value &json)
{
	if (json.isNull())
		return;

	std::string name	= json["name"].asString(),
				title	= json["title"].asString();

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
	_analysisId			= json["analysisId"].asInt();
	_constructorJson	= json["constructorJson"];
	_autoSortByValue	= json["autoSortByValue"].asBool();

	db().columnSetComputedInfo(_id, _analysisId, _invalidated, _codeType, _rCode, _error, constructorJsonStr());
	
	deserializeLabelsForCopy(json["labels"]);

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
	
	dbUpdateValues(false);
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

qsizetype Column::getMaximumWidthInCharactersIncludingShadow()
{
	bool thereIsAShadow = false;
	
	//If there are no labels there no shadows? 
	for(Label * label : labels())
		if(label->originalValueAsString(true) != label->label())
		{
			thereIsAShadow = true;
			break;
		}
		
	if(!thereIsAShadow)
		return getMaximumWidthInCharacters(true, false);
	
	return getMaximumWidthInCharacters(true, false) + getMaximumWidthInCharacters(true, true, 0);
}


qsizetype Column::getMaximumWidthInCharacters(bool fancyEmptyValue, bool valuesPlease, qsizetype	extraPad)
{
	qsizetype	maxWidth	= 0;
	std::string takeWidth;
	
	//Call labelsTempCount() to both find out how many there are and generate them if necessary
	bool thereAreLabels = labelsTempCount() > 0;
	
	if(thereAreLabels)
		for(Label * label : labels())
		{
			takeWidth	= !valuesPlease ? label->label() : label->originalValueAsString(fancyEmptyValue);
			maxWidth	= std::max(maxWidth, qsizetype(stringUtils::approximateVisualLength(takeWidth)));
		}
	
	
	return std::max(maxWidth, _labelsTempMaxWidth) + extraPad;
}

stringvec Column::previewTransform(columnType transformType)
{
	const int showThisMany = 16;
	stringvec out;
	
	out.push_back(std::to_string(labelsTempCount()));
	out.push_back(std::to_string(_labelsTempNumerics));
	
	{
		std::stringstream someValues;
		
		
		for(int count = 0; count < _ints.size() && count < showThisMany; count++)
			someValues << (count > 0 ? ", " : "") << (transformType == columnType::scale ? getValue(count, true, false, transformType) : '"' + getLabel(count, true) + '"');
		
		if(_ints.size() > showThisMany)
			someValues << ", ...";
		
		out.push_back(someValues.str());
	}
	
	{
		std::stringstream someImplicitEmptyValues;
		
		if(transformType == columnType::scale && labelsTempCount() > _labelsTempNumerics)
		{
			int count = 0;
			
			for(Label * label : _labels)
				if(!label->isEmptyValue() && !ColumnUtils::isDoubleValue(label->originalValueAsString()))
				{
					if(count < showThisMany)
						someImplicitEmptyValues << (count > 0 ? ", " : "") << '"' << label->originalValueAsString() << '"';
					else
					{
						someImplicitEmptyValues << ", ...";
						break; // Do not need to loop further over the labels.
					}

					count++;
				}	
		}
		
		out.push_back(someImplicitEmptyValues.str());
	}
	
	return out;
}


