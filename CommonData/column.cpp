#include "log.h"
#include <cassert>
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
	_autoSortByValue(	_autoSortByValuesByDefault)
{
	if(_id != -1)
		db().columnSetAutoSort(_id, _autoSortByValue); //Store autosort in db
}

Column::~Column()
{
	delete _emptyValues;
}

void Column::dbCreate(int index)
{
	JASPTIMER_SCOPE(Column::dbCreate);

	assert(_id == -1);
	db().columnInsert(_id, index);
	assert(_id != -1);
}

void Column::dbLoad(int id, bool getValues)
{
	JASPTIMER_SCOPE(Column::dbLoad);

	assert(_id == id || (id != -1 && _id == -1) || _id != -1);

	if(id != -1)
		_id = id;

	db().transactionReadBegin();
	
	Json::Value			emptyVals;
	int					dropLevelsTypeInt	= static_cast<int>(_dropLevels);
	std::string			oldError			= _error;
	
	db().columnGetBasicInfo(	_id, _name, _title, _description, _type, _revision, emptyVals, _autoSortByValue, dropLevelsTypeInt, _hasLabels);
	db().columnGetComputedInfo(	_id, _analysisId, _invalidated, _codeType, _rCode, _error, _constructorJson, _computeFilter);
	
	try { _dropLevels = dropLevelsType(dropLevelsTypeInt); } catch(...){}
	
	_emptyValues->fromJson(emptyVals);
	
	if(getValues)
	{
		if(_hasLabels)
		{
			db().labelsLoad(this);
			db().columnGetValues(_id, _ints);
		}
		else
		{
			db().columnGetValues(_id, _dbls, _strs);
		}
	}

	_resetLabelValueMap();

	db().transactionReadEnd();
}

void Column::dbLoadOldIndex(int index)
{
	JASPTIMER_SCOPE(Column::dbLoadOldIndex);
	
	_id = db().columnIdForIndex(_data->id(), index);

	assert(_id != -1);

	db().transactionWriteBegin();
	
	dbLoad(_id, false);
	
	db().columnGetValues(_id, _ints,		"INT");
	db().columnGetValues(_id, _dbls, _strs, "DBL");
	
	if(true)
	{
		JASPTIMER_START(Column::dbLoadOldIndex look for trouble);
		
		bool					thisCantBeRight = false;
		std::map<int, double>	lookForTrouble; //0.18 messed up some things, and maybe 0.19, make sure we only import logical labels. These messed up things could also be in upgraded jaspfiles...
		
		for(size_t r=0; r<_ints.size() && !thisCantBeRight; r++)
		{			
			if(lookForTrouble.count(_ints[r]))
			{
				if(lookForTrouble.at(_ints[r]) != _dbls[r] && !(std::isnan(_dbls[r]) && std::isnan(lookForTrouble.at(_ints[r]))))
					thisCantBeRight = true;
			}
			else
				lookForTrouble[_ints[r]] = _dbls[r];
		}
		
		JASPTIMER_STOP(Column::dbLoadOldIndex look for trouble);
		
		//Turns out one label is used in conjunction with more than 1 value... That cant be right, so lets throw away all these integers		
		if(thisCantBeRight)
			for(size_t r=0; r<_ints.size(); r++)
				_ints[r] = Label::NO_LABEL;
		else // we should still check if these labels even exist and otherwise clean that up too
		{
			intset existingLabels = db().labelsExisting(_id);
			
			for(size_t r=0; r<_ints.size(); r++)
				if(existingLabels.count(_ints[r]) == 0)
					_ints[r] = Label::NO_LABEL;
		}
	}
	
	if(std::all_of(_ints.begin(), _ints.end(), [](int i){ return i == Label::NO_LABEL || i == EmptyValues::missingValueInteger; }))
	{
		JASPTIMER_START(Column::dbLoadOldIndex remove all labels);
		
		_hasLabels = false;
		_ints.clear();
		labelsClear();
		db().labelsClear(_id);
		
		for(int row=0; row<_strs.size() && row < _dbls.size(); row++)
		{
			//Log::log() << "_strs["<< row << "] == " << _strs[row] << " and  _dbls["<< row << "] == " << _dbls[row]  << std::endl;
			
			double dbl;
			
			if(ColumnUtils::getDoubleValue(_strs[row], dbl, false) && ((std::isnan(dbl) && std::isnan(_dbls[row])) || _dbls[row] == dbl))
				_strs[row] = "";
		}
		
		JASPTIMER_STOP(Column::dbLoadOldIndex remove all labels);
		
		
	}
	else
	{
		JASPTIMER_START(Column::dbLoadOldIndex load all labels);

		
		_hasLabels = true;
		
		db().labelsLoad(this);
		_resetLabelValueMap();
		
		_strs.clear();
		_dbls.clear();
		
		
		bool everythingIsTheSame = true;
		
		for(Label * label : _labels)
			if(label->originalValueAsString(false, true) != label->label(false))
			{
				everythingIsTheSame = false;
				break;
			}
		
		if(everythingIsTheSame)
			labelsToNoLabels(false);
		
		JASPTIMER_STOP(Column::dbLoadOldIndex load all labels);
	}
	
	db().columnSetHasLabels(_id, _hasLabels);
	incRevision();
	
	db().transactionWriteEnd();
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

	if (orgName == _name) // Special case when the getUniqueName gives back the original name (e.g 2 columns 'test' and 'test 2' and the user rename 'test 2' to 'test')
		return false;

	if(!_title.empty() && (_title == orgName || _title == _name)) 
		setTitle(_name);

	db().columnSetName(_id, _name);
	incRevision();

	return true;
}

void Column::setTitle(const std::string &title)
{
	JASPTIMER_SCOPE(Column::setTitle);
	
	if(Column::title() == title)
		return;

	_title = _name != title ? title : "";
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

void Column::setComputeFilter(const std::string &filter)
{
	JASPTIMER_SCOPE(Column::setComputeFilter);

	if(_computeFilter == filter)
		return;

	_computeFilter = filter;
	invalidate();
	db().columnSetComputeFilter(_id, _computeFilter);
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
	
	nonFilteredCountersReset();
	
	incRevision();
}

bool Column::setCustomEmptyValues(const stringset& customEmptyValues)
{
	JASPTIMER_SCOPE(Column::setCustomEmptyValues);
	
	if (_emptyValues->emptyStrings() == customEmptyValues)
		return false;

	_emptyValues->setEmptyValues(customEmptyValues, _emptyValues->hasEmptyValues());
	db().columnSetEmptyVals(_id, _emptyValues->toJson().toStyledString());

	nonFilteredCountersReset();
	
	incRevision();
	
	return true;
}

void Column::dbUpdateComputedColumnStuff()
{
	db().columnSetComputedInfo(_id, _analysisId, _invalidated, _codeType, _rCode, _error, constructorJsonStr(), _computeFilter);
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

	if(_codeType == computedColumnType::analysisNotComputed && codeType != computedColumnType::analysis)
		_analysisId = -1;

	Log::log() << "Column " << _name << "'s codeType changes from " << computedColumnTypeToString(_codeType) << " to " << computedColumnTypeToString(codeType) << std::endl;
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

	if(columnType != columnType::unknown)
		setType(columnType);
	
	for(size_t i=0; i<rowCount(); i++)
		if(_hasLabels)		setValue(i, EmptyValues::missingValueInteger,		false);
	else					setValue(i, EmptyValues::missingValueDouble,	"", false);
	
	labelsClear();
	
	dbUpdateValues();
}

void Column::setDropLevels(dropLevelsType dropEm)
{
	JASPTIMER_SCOPE(Column::setDropLevels);
	
	if(_dropLevels == dropEm)
		return;
	
	_dropLevels = dropEm;
	
	db().columnSetDropLevels(_id, static_cast<int>(_dropLevels));
	
	incRevision();
}

void Column::dbUpdateValues()
{
	if(!_data->writeBatchedToDB())
	{
		if(_hasLabels)
			db().columnSetValues(_id, _ints);
		else
			db().columnSetValues(_id, _dbls, _strs);
	}
	
	incRevision();
}

columnType Column::resetValues(int thresholdScale)
{
	return setValues(valuesAsStrings(), labelsAsStrings(), thresholdScale);
}

stringset Column::mergeOldMissingDataMap(const Json::Value &missingData)
{
	stringset foundEmpty;
	
	std::map<std::string, Label*> displayToLabel; //Keep track of which labels we added because only those could possibly be derived from missingDataMap
	for(size_t r=0;	r<_ints.size(); r++)
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
									_ints[r]		= label ? label->intsId() : -1;
									_dbls[r]		= dbl;
		}
	}
	
	foundEmpty.erase(""); //So for some currently inscrutable reason empty strings were also stored in the missing data map... Remove any occurences.
	
	return foundEmpty;
}

columnType Column::setValues(const stringvec & values, const stringvec & labels, int thresholdScale, bool * aChange, bool useLocale, bool determineWhetherOneWantsLabels)
{
	assert(values.size() == labels.size() || labels.size() == 0);
	
	return setValues(
				values.size(),
				[&values](size_t r){return values[r];},
				[&labels](size_t r){return r < labels.size() ? labels[r] : "";},
				thresholdScale,
				aChange,
				useLocale,
				determineWhetherOneWantsLabels
				);
}
				
columnType Column::setValues(size_t rows, const std::function<std::string(size_t)> valueLookup, const std::function<std::string(size_t)> labelLookup, int thresholdScale, bool * aChange, bool useLocale, bool determineWhetherOneWantsLabels)
{
	JASPTIMER_SCOPE(Column::setValues);

	if(aChange && rowCount() != rows)
		(*aChange) = true;

	size_t prevSize = rowCount();
	
	bool	onlyDoubles = true, 
			onlyInts	= true;
	
	//Weve already made sure we have only 1 label per value and display combo, because otherwise this will get too complicated

	intset	ints;  // to suggest whether this is a scalar or not we need to know whether we have more than threshold ints or not.
	int		tmpInt;
	double	tmpDbl;
	
	
	bool allTheSame = true;
	
	if(determineWhetherOneWantsLabels)
	{
		//Should we have labels?
		for(int r=0; r<rows && allTheSame; r++)
			if(valueLookup(r) != labelLookup(r) && labelLookup(r) != "")
				allTheSame = false;
	}
	else
	{
		allTheSame = !_hasLabels;	
	}
	
	if(allTheSame)
	{
		setHasLabels(false);
		_hasShadows = false;
		_maxWidthValue = 0;
		_maxWidthLabel = 0;
		
		_dbls.resize(rows);
		_strs.resize(rows);
		
		for(size_t resetRow=prevSize; resetRow<_ints.size(); resetRow++)
		{
			_dbls[resetRow]	= EmptyValues::missingValueDouble;
			_strs[resetRow] = "";
		}
	
		JASPTIMER_RESUME(Column::setValues call setValue and count integers);
		
		for(size_t i=0; i<rows; i++)
		{
			std::string	valueStr = valueLookup(i);
			double		valueDbl = EmptyValues::missingValueDouble;
			bool		isDouble = ColumnUtils::getDoubleValue(valueStr, valueDbl);

			_maxWidthValue = std::max(_maxWidthValue, int(stringUtils::approximateVisualLength(valueStr)));
		
			if(setValue(i, valueDbl, isDouble ? "" : valueStr, false) && aChange)
				(*aChange) = true;
					
			if(!isDouble)
			{
				if(!isEmptyValue(valueStr))
				{
					onlyInts	= false;
					onlyDoubles = false;
				}
			}
			else if(!isEmptyValue(valueDbl))
			{
				if(doubleToDisplayString(valueDbl) == doubleToDisplayString(double(int(valueDbl))) && int(valueDbl) != EmptyValues::missingValueInteger)
					ints.insert(int(valueDbl));
				else
					onlyInts = false;
			}
		}

		_maxWidthLabel = _maxWidthValue;
	}
	else
	{
		setHasLabels(true);
		
		_ints.resize(rows);
		
		for(size_t resetRow=prevSize; resetRow<_ints.size(); resetRow++)
			_ints[resetRow]	= EmptyValues::missingValueInteger;

		JASPTIMER_RESUME(Column::setValues call setValue and count integers);
		
		for(size_t i=0; i<rows; i++)
		{
			const std::string	value = valueLookup(i),
								label = labelLookup(i);
			
			if(setValue(i, value, label, false, useLocale) && aChange)
				(*aChange) = true;
					
			if(value != "" || label != "")
			{
				if(ColumnUtils::getIntValue(value, tmpInt))
					ints.insert(tmpInt);
				else if(!isEmptyValue(value))
					onlyInts = false;
				
				if(!ColumnUtils::getDoubleValue(value, tmpDbl, useLocale) && !isEmptyValue(value))
					onlyDoubles = false;
			}
		}
		
		if(labelsRemoveOrphans() && aChange)
			(*aChange) = true;
		
		JASPTIMER_STOP(Column::setValues call setValue and count integers);
	}
	
	dbUpdateValues();

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
	std::set<Label*> labelsNonNA;
	
	JASPTIMER_RESUME(Column::setValues count labels);
	
	for(size_t i=0; i<_ints.size(); i++)
	{
		int value = _ints[i];

		Label * label = labelByIntsId(value);
		
		if(label && !label->isEmptyValue())
			labelsNonNA.insert(label);
	}
	
	JASPTIMER_STOP(Column::setValues count labels);
	
	size_t howManyLabelLike = labelsNonNA.size();

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
					if(_ints[row] != Label::NO_LABEL && _ints[row] != labelValue)
						Log::log() << "Column(" << name() << ")::setDescriptions(...)\n" << "_ints[" << row << "] != Label::NO_LABEL && _ints[" << row << "] != labelValue (" << labelValue << ")" << std::endl;
					_ints[row] = labelValue;
				}
		}
	}
	
	return anyChange;
}

bool Column::overwriteDataAndType(stringvec colData, columnType colType, bool computed)
{
	JASPTIMER_SCOPE(Column::overwriteDataAndType);
	
	if(computeFilter() != "")
	{
		Filter theFilter(data(), computeFilter(), false);
		
		const boolvec & filtered = theFilter.filtered();
		stringvec		newData;
						newData	 . reserve(filtered.size());
		
		for(size_t iFilter=0, iData=0; iFilter < filtered.size() && iData < colData.size(); iFilter++)
			newData.push_back(filtered[iFilter] ? colData[iData++] : "");
			
		colData = newData;
	}
	
	Log::log() << "Column " << _name << " overwriteDataAndType(" << colData.size() << " rows of data, "<<columnTypeToString(colType)<<", bool computed=" << (computed ? "true" : "false") << ")" << std::endl;
	
	//Now to make sure that the colData is neither bigger nor smaller than the dataset:
	colData.resize(data()->rowCount()); //Either add blanks rows add end or drop superfluous data
	
	bool			changes		= _type != colType,
					toScale		= colType == columnType::scale;
	stringvec		otherData	= colData,
				&	values		= toScale  ? colData : otherData,
				&	labels		= !toScale ? colData : otherData;
	
	
	if(_hasLabels)
	{
		
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
		
		for(size_t i=0; i<colData.size(); i++)
			otherData[i] = getOther(colData[i]);
		
		// overwriteDataAndType is called only by the Engine, when R computes some values for a column (computed columns)
		// In this case, the locale used is just UTF-8/C, so the locale chosen by the user should not be used to convert the values.
		beginBatchedLabelsDB();
		setValues(values, labels, 0, &changes, false);
		nonFilteredCountersReset();
		labelsHandleAutoSort();
		endBatchedLabelsDB();
	}
	else // no labels
	{
		setValues(colData, colData, 0, &changes, false);
	}
	
	if(computed && _codeType == computedColumnType::analysisNotComputed && _analysisId != -1)
		setCodeType(computedColumnType::analysis);
	setType(colType);
	
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
	
	db().labelsSetOrder(_updateNonEmptyIndexesAndLabelOrder());
	
	incRevision();
}

void Column::_sortLabelsByOrder()
{
	std::sort(_labels.begin(), _labels.end(), [](const Label * l, const Label * r) { return l->order() < r->order(); });
}

void Column::labelsClear(bool doIncRevision)
{
	bool hadLabels = _labels.size() > 0;
	for (Label* label : _labels)
		delete label;
	
	if(hadLabels)
		db().labelsClear(_id);
	
	_labelNonEmptyIndexByLabel.clear();
	_labelByNonEmptyIndex.clear();
	_labelByIntsIdMap.clear();
	_labelsByDisplay.clear();
	_labelsByValue.clear();
	_labelByValDis.clear();
	_labels.clear();
	
	_maxWidthLabel	= -1;
	_maxWidthValue	= -1;
	_hasShadows		= false;
	
	_highestIntsId = 0;
	
	if(doIncRevision)
		incRevision();
}


void Column::_resetLabelValueMap()
{

	_labelByIntsIdMap.clear();
	_labelByValDis.clear();
	_labelsByValue.clear();
	_labelsByDisplay.clear();
	
	_maxWidthLabel	= -1;
	_maxWidthValue	= -1;
	_hasShadows		= false;
	
	_highestIntsId = 0;
	
	
	
	for(Label * label : _labels)
		_labelMapIt(label);
}


void Column::beginBatchedLabelsDB()
{
	_batchedLabelDepth++;
}

void Column::endBatchedLabelsDB(bool wasWritingBatch)
{
	JASPTIMER_SCOPE(Column::endBatchedLabelsDB);
	
	assert(_batchedLabelDepth > 0);
	_batchedLabelDepth--;
	
	_updateNonEmptyIndexesAndLabelOrder();
	
	if(_batchedLabelDepth == 0)
	{
		if(wasWritingBatch)
		{
			db().labelsWrite(this);
			incRevision(); //Should trigger reload at engine end
		}
	}	
}

intintmap Column::_updateNonEmptyIndexesAndLabelOrder()
{
	//Set label non empty stuff based the inherent order in _labels
	_labelNonEmptyIndexByLabel	.clear();
	_labelByNonEmptyIndex		.clear();

	intintmap	orderPerDbIds;
	size_t		nonEmptyIndex	= 0;
				_highestIntsId	= 0;
				
	for(size_t i=0; i<_labels.size(); i++)
	{
		_labels[i]->setOrder(i);
		
		orderPerDbIds[_labels[i]->dbId()] = i;

		_highestIntsId = std::max(_highestIntsId, _labels[i]->intsId());
		
		if(!_labels[i]->isEmptyValue())
		{
			_labelNonEmptyIndexByLabel[_labels[i]]	= nonEmptyIndex;
			_labelByNonEmptyIndex[nonEmptyIndex]	= _labels[i];
			nonEmptyIndex++;
		}
	}
	
	return orderPerDbIds;
}

int Column::labelsAdd(int display)
{
	return labelsAdd(std::to_string(display));
}

int Column::labelsAdd(double display)
{
	return labelsAdd(doubleToDisplayString(display, false, true));
}

int Column::labelsAdd(const std::string &display)
{
	return labelsAdd(display, display);
}

int Column::labelsAdd(const std::string &display, const std::string &value)
{

	JASPTIMER_SCOPE(Column::labelsAdd displaystring);

	if(display == "")
		return EmptyValues::missingValueInteger;
	
	int		anInt;
	double	aDouble;
	
	Json::Value original = value;
	
	if		(ColumnUtils::getIntValue(		value, anInt))		original = anInt;
	else if	(ColumnUtils::getDoubleValue(	value, aDouble))	original = aDouble;

	return labelsAdd(display, "", original);
}

int Column::labelsAdd(const std::string & display, const std::string & description, const Json::Value & originalValue)
{
	JASPTIMER_SCOPE(Column::labelsAdd 3 args);
	
	std::string oriString			= Label::originalValueAsString(this, originalValue),
				displayProcessed	= Label::processLabel(display, oriString);
	auto		valDisplay			= std::make_pair(oriString, displayProcessed);

	if(_labelByValDis.count(valDisplay))
		return _labelByValDis.at(valDisplay)->intsId();

	return labelsAdd(++_highestIntsId, display, true, description, originalValue);
}

int Column::_labelMapIt(Label * label)
{
	_labelByIntsIdMap	[ label->intsId()				] =			label;
	_labelsByDisplay	[ label->label()				].insert(	label);
	_labelByValDis		[ label->origValDisplay()		] =			label;
	_labelsByValue		[ label->originalValueAsString()].insert(	label);
	
	label->rememberCurrentOrigValDisplay(); 

	_highestIntsId = std::max(_highestIntsId, label->intsId());
	_maxWidthValue = std::max(_maxWidthValue, int(stringUtils::approximateVisualLength(label->originalValueAsString(true, false))));
	_maxWidthLabel = std::max(_maxWidthLabel, int(stringUtils::approximateVisualLength(label->label())));

	if(label->originalValueAsString() != label->label() && !label->isEmptyValue())
		_hasShadows = true;
		
	return label->intsId();
}

int Column::labelsAdd(int value, const std::string & display, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order, int id)
{
	JASPTIMER_SCOPE(Column::labelsAdd lotsa arg);

	std::string oriString			= Label::originalValueAsString(this, originalValue),
				displayProcessed	= Label::processLabel(display, oriString);
	auto		valDisplay			= std::make_pair(oriString, displayProcessed);

	if(_labelByValDis.count(valDisplay))
		return _labelByValDis.at(valDisplay)->intsId();


	Label * label = new Label(this, display, value, filterAllows, description, originalValue, order, id);
	_labels.push_back(label);

	return _labelMapIt(label);
}

void Column::labelsRemove(int labelIndex)
{
	assert(_hasLabels);
	
	if(_labels.size() > labelIndex)
	{
		Label * label = _labels[labelIndex];
		
		int intsId = label->intsId();
		
		labelsRemoveByIntsId({intsId}, false);
		
		for(size_t i=0; i<_ints.size(); i++)
			if(_ints[i] == intsId)
				_ints[i] = EmptyValues::missingValueInteger;
	}
	
	db().columnSetValues(_id, _ints);
	nonFilteredCountersReset();
	_dbUpdateLabelOrder();
	
	incRevision();
}

int Column::labelsSet(int labelIndex, int value, const std::string &display, bool filterAllows, const std::string &description, const Json::Value &originalValue, int order, int id)
{
	JASPTIMER_SCOPE(Column::labelsSet);
	assert(_hasLabels);

	Label * label = nullptr;
	if(_labels.size() > labelIndex)
	{
		label = _labels[labelIndex];
		label->setInformation(this, id, order, display, value, filterAllows, description, originalValue);
	}
	else
	{
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
	assert(_hasLabels);

	JASPTIMER_SCOPE(Column::labelsRemoveByIntsId);

	_labels.erase(
		std::remove_if(
			_labels.begin(),
			_labels.end(),
			[&](Label * label) {
				if(std::find(valuesToRemove.begin(), valuesToRemove.end(), label->intsId()) != valuesToRemove.end())
				{
					_labelByIntsIdMap.erase(label->intsId());
					
					auto valDis = label->origValDisplay();
					if(_labelByValDis.count(valDis) && _labelByValDis.at(valDis) == label)
						_labelByValDis.erase(valDis);
						
					if(_labelsByValue.count(valDis.first))
						_labelsByValue.at(valDis.first).erase(label);
					
					if(_labelsByDisplay.count(valDis.second))
						_labelsByDisplay.at(valDis.second).erase(label);
						
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

void Column::labelsShrinkOnlyToSize(size_t newSize)
{
	assert(_hasLabels);
	
	for(size_t i=newSize; i<_labels.size(); i++)
		delete _labels[i];
	
	if(newSize < _labels.size())
		_labels.resize(newSize);

	_resetLabelValueMap();
}

int Column::nonFilteredNumericsCount()
{
	if (_nonFilteredNumericsCount == -1)
	{
		JASPTIMER_SCOPE(Column::nonFilteredNumericsCount);
		
		doubleset numerics;

		for(size_t r=0; r<_data->rowCount(); r++)
		{
			if(_hasLabels)
			{
				Label * l = labelByIntsId(_ints[r]);
				
				if(_data->filter()->filtered()[r] && l && !l->isEmptyValue())
					numerics.insert(l->originalValueAsDouble());
			}
			else
				if(_data->filter()->filtered()[r] && !isEmptyValue(_dbls[r]))
					numerics.insert(_dbls[r]);
		}

		if(_hasLabels && !shouldDropLevels())
			for(Label * label : _labels)
				if(!std::isnan(label->originalValueAsDouble()) && !label->isEmptyValue()) //This was originally just a emptyValue check, but why would we want the empty values in particular?
					numerics.insert(label->originalValueAsDouble());

		_nonFilteredNumericsCount = numerics.size();
	}

	return _nonFilteredNumericsCount;
}


int Column::nonFilteredNumericsCount() const
{
#ifdef JASP_DEBUG
	if(_nonFilteredNumericsCount == -1)
		throw std::runtime_error("nonFilteredNumericsCount() const was not initialized");
#endif
	
	return _nonFilteredNumericsCount;
}


const stringvec & Column::nonFilteredLevels() 
{
	if (_nonFilteredLevels.empty())
	{
		JASPTIMER_SCOPE(Column::nonFilteredLevels);
		if(_hasLabels)
		{
			stringset 	levels;
			intset		collected;
	
			for(size_t r=0; r<data()->rowCount(); r++)
				if(data()->filter()->filtered()[r])
					if(_ints[r] != Label::NO_LABEL)
					{
						if(!collected.count(_ints[r]))
						{
							Label * label = labelByIntsId(_ints[r]);
							if(label && !label->isEmptyValue())
								levels.insert(label->label());
							
							collected.insert(_ints[r]);
						}
					}
	
			if(!shouldDropLevels())
				for(Label * label : _labels)
					levels.insert(label->label());
	
			// Use the right label order
			for(Label * label : _labels)
				if (levels.count(label->label()))
					_nonFilteredLevels.push_back(label->label());
		}
		else
		{
			intvec intVals;
			_nonFilteredLevels = dataAsRLevels(intVals, data()->filter()->filtered());
		}
	}

	return _nonFilteredLevels;
}

const stringvec & Column::nonFilteredLevels() const
{
	return _nonFilteredLevels;
}
 

void Column::nonFilteredCountersReset(bool updateLabelIndexes)
{
	_nonFilteredLevels.clear();
	_nonFilteredNumericsCount = -1;
	
	if(updateLabelIndexes)
		_updateNonEmptyIndexesAndLabelOrder();
}

int Column::labelIndexNonEmpty(Label *label) const
{			
	return !_labelNonEmptyIndexByLabel.count(label) ? -1 : _labelNonEmptyIndexByLabel.at(label);
}

Label * Column::labelByIndexNonEmpty(int index) const
{
	return !_labelByNonEmptyIndex.count(index) ? nullptr : _labelByNonEmptyIndex.at(index);
}


size_t Column::labelsNonEmptyCount() const
{
	return _labelNonEmptyIndexByLabel.size();
}

std::string Column::_getLabelDisplayStringByValue(int key, bool ignoreEmptyValue) const
{
	if (key == EmptyValues::missingValueInteger)
		return EmptyValues::displayString();
	
	if(_labelByIntsIdMap.count(key))
	{
		return	ignoreEmptyValue 
			?	_labelByIntsIdMap.at(key)->label()
			:	_labelByIntsIdMap.at(key)->labelDisplay();
	}

	return std::to_string(key);
}

std::string Column::getDisplay(size_t row, bool fancyEmptyValue, bool sepas) const
{
	if(!_hasLabels)
		getValue(row, fancyEmptyValue, false, sepas);
	
	return _type == columnType::scale	
		?	getValue(row, fancyEmptyValue, false, sepas)
		:	getLabel(row, fancyEmptyValue, false, sepas);
}

std::string Column::getShadow(size_t row, bool fancyEmptyValue, bool sepas) const
{
	if(!_hasLabels && _type == columnType::scale)
	{
		std::string valueNotSoFancy = getValue(row, false, false, sepas);
		
		return valueNotSoFancy == "" ? getLabel(row, fancyEmptyValue, true, sepas) : "";
	}
	
	return _type != columnType::scale	
		?	getValue(row, fancyEmptyValue, true, sepas)
		:	getLabel(row, fancyEmptyValue, true, sepas);
}

std::string Column::getValue(size_t row, bool fancyEmptyValue, bool ignoreEmptyValue, bool sepas, columnType asType) const
{
	if(asType == columnType::unknown)
		asType = _type;
	
	if (row < rowCount())
	{
		if(_hasLabels)
		{
			if (_ints[row] != EmptyValues::missingValueInteger)
			{
				Label * label = labelByIntsId(_ints[row]);
	
				if(label)
				{
					if (asType == columnType::scale)
						return doubleToDisplayString(label->originalValueAsDouble(), fancyEmptyValue, ignoreEmptyValue, sepas);
					return label->originalValueAsString(fancyEmptyValue, ignoreEmptyValue);
				}
			}
		}
		else
		{
			const double		& _dbl = _dbls[row];
			const std::string	& _str = _strs[row];
			
			if (asType == columnType::scale)
				return doubleToDisplayString(_dbl, fancyEmptyValue, ignoreEmptyValue, sepas);
			
			return _str != ""
					? (ignoreEmptyValue || !isEmptyValue(_str) ? _str : "") 
					: doubleToDisplayString(_dbl, fancyEmptyValue, ignoreEmptyValue, sepas);
		}
	}
	
	return fancyEmptyValue ? EmptyValues::displayString() : "";
}

std::string Column::getLabel(size_t row, bool fancyEmptyValue, bool ignoreEmptyValue, bool sepas) const
{
	if(!_hasLabels)
	{
		const double		& _dbl = _dbls[row];
		const std::string	& _str = _strs[row];

		return _str != ""
				? (ignoreEmptyValue || !isEmptyValue(_str) ? _str : "")
				: doubleToDisplayString(_dbl, fancyEmptyValue, ignoreEmptyValue, sepas);
	}
	
	if (row < rowCount() && _ints[row] != Label::NO_LABEL)
			return _getLabelDisplayStringByValue(_ints[row], ignoreEmptyValue);
		
	return fancyEmptyValue ? EmptyValues::displayString() : "";
}


std::string Column::doubleToDisplayString(double dbl, bool fancyEmptyValue, bool ignoreEmptyValue, bool sepas) const
{
	ignoreEmptyValue = ignoreEmptyValue && !std::isnan(dbl);
	
	if (isEmptyValue(dbl) && !ignoreEmptyValue)				return fancyEmptyValue ? EmptyValues::displayString() : "";
	else													return ColumnUtils::doubleToString(dbl, sepas);
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


stringvec Column::nonEmptyLevelsStrings() const
{
	if(!_hasLabels)
		return {};
	
	stringvec returnMe;
	returnMe.resize(labelsNonEmptyCount());
	
	for(size_t i=0; i<returnMe.size(); i++)
		returnMe[i] = labelByIndexNonEmpty(i)->label();
	
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

stringvec Column::dataAsRLevels(intvec & values, const boolvec & filter)
{
	JASPTIMER_SCOPE(Column::dataAsRLevels);

	assert(filter.size() == rowCount() || filter.size() == 0);

	//We ignore emptyvalues and depending on whether filter is usable (length is data length) we filter out rows we dont need
	const bool 	useFilter 	= filter.size() == rowCount();

	if(_hasLabels)
	{
		int  		valuesSize	= 0;
		intset		ids;
		stringset	usedLevels; //https://github.com/jasp-stats/jasp-issues/issues/3721 points out rightly that if you map multiple values to the same label, they should then be 1 label in ordinal/nominal!
	
		for(size_t row=0; row<rowCount(); row++)
			if(!useFilter || filter[row])
			{
				valuesSize++;
				ids.insert(_ints[row]);
			}
		
		for(int id : ids)
			if(id != Label::NO_LABEL && id != EmptyValues::missingValueInteger)
			{
				Label * label = labelByIntsId(id);
	
				if(label && !label->isEmptyValue())
					usedLevels.insert(label->label());
			}
		
		stringvec levels;
		levels.reserve(usedLevels.size());
	
		strintmap labelToLevel;
	
		for(Label * label : _labels)
			if(!labelToLevel.count(label->label()) && (usedLevels.count(label->label()) || !shouldDropLevels()))
			{
				levels.push_back(label->label());
				labelToLevel[label->label()] = levels.size();
			}
			
		values.resize(valuesSize);
	
		for(size_t row=0, valueRow=0; row<rowCount() && valueRow < valuesSize; row++)
			if(!useFilter || filter[row])
			{
				values[valueRow] = EmptyValues::missingValueInteger;
	
				if(_ints[row] != Label::NO_LABEL && _ints[row] != EmptyValues::missingValueInteger)
				{
					Label * label = labelByIntsId(_ints[row]);
	
					if(label && labelToLevel.count(label->label()))
						values[valueRow] = labelToLevel.at(label->label());
				}
				
				valueRow++;
			}
		
		return levels;
	}
	else // if !_hasLabels
	{
		stringvec	strs = _strs;
		stringset	levelSet;
		
		std::map<std::string, double> weWereDoublesToBeginWith;
		
		for(size_t r=0; r<strs.size(); r++)
			if(!useFilter || filter[r])
				if(strs[r] == "")
				{
					strs[r] = doubleToDisplayString(_dbls[r], false, false, false);
					weWereDoublesToBeginWith[strs[r]] = _dbls[r];
				}
		
		for(size_t r=0; r<strs.size(); r++)
			if(!useFilter || filter[r])
				levelSet.insert(strs[r]);
		
		levelSet.erase("");
		
		stringvec levels(levelSet.begin(), levelSet.end());
		
		std::sort(levels.begin(), levels.end(), [&weWereDoublesToBeginWith](const std::string & l, const std::string & r)
		{
			if(weWereDoublesToBeginWith.count(l) && weWereDoublesToBeginWith.count(r))
				return weWereDoublesToBeginWith.at(l) < weWereDoublesToBeginWith.at(r);
			else if(weWereDoublesToBeginWith.count(l))
				return true;
			else  if(weWereDoublesToBeginWith.count(r))
				return false;
			else 
				return l < r;
		});
		
		std::map<std::string, int> levelToInt;
		for(int i=0; i<levels.size(); i++)
			levelToInt[levels[i]] = i+1;
		
		values.clear();
		for(size_t r=0; r<strs.size(); r++)
			if(!useFilter || filter[r])
				values.push_back(strs[r] == "" ? EmptyValues::missingValueInteger : levelToInt[strs[r]]);

		return levels;		
	}
}

doublevec Column::dataAsRDoubles(const boolvec &filter) const
{
	JASPTIMER_SCOPE(Column::dataAsRDoubles);
	
	if(!_hasLabels)
	{
		if(filter.size() == 0)
			return _dbls;
		
		assert(filter.size() == rowCount());
		
		doublevec dbls;
		for(size_t row=0; row<rowCount(); row++)
			if(filter[row])
				dbls.push_back(_dbls[row]);
		return dbls;
	}
	
	doublevec doubles;
		
	assert(filter.size() == rowCount() || filter.size() == 0);

	//depending on whether filter is usable (length is data length) we filter out rows we dont need
	bool useFilter = filter.size() == rowCount();
	doubles.reserve(_ints.size());
	
	for(size_t row=0; row<rowCount(); row++)
		if(!useFilter || filter[row])
		{
			Label * l = labelByRow(row);
			doubles.push_back(l && !l->isEmptyValue() ? l->originalValueAsDouble() : EmptyValues::missingValueDouble);
		}
				
	return doubles;
}

void Column::labelValueChanged(Label *label, const Json::Value & previousOriginal)
{
	assert(_hasLabels);
	
	auto prevOrigV	= Label::originalValueAsString(this, previousOriginal);
	auto oldValDis	= std::make_pair(prevOrigV, label->label());
	bool merged		= _labelByValDis.count(label->origValDisplay()) != 0;
	
	if(merged && _labelByValDis.at(label->origValDisplay()) != label)
		labelsMergeDuplicateInto(label);
	
	//Make sure it was registered before:
	assert(_labelByValDis[label->lastOrigValDisplay()] == label);
	//And that its new location is free:
	assert(_labelByValDis.count(label->origValDisplay()) == 0 || _labelByValDis.at(label->origValDisplay()) == label);

	_labelMapUpdates(label, label->lastOrigValDisplay().second, label->lastOrigValDisplay().first);

	if(merged)
		_dbUpdateLabelOrder();

	nonFilteredCountersReset();
	
	dbUpdateValues();
}


void Column::_handleWidthChangeWithoutLabels()
{
	_hasShadows		= false;
	_maxWidthValue	= 0;
	_maxWidthLabel	= 0;

	for(int r=0; r<_dbls.size(); r++)
	{
		const std::string	v = getValue(r),
							l = getLabel(r);

		uint64_t	lv = stringUtils::approximateVisualLength(v),
					ll = stringUtils::approximateVisualLength(l);

		_maxWidthValue = std::max(_maxWidthValue, int(lv));
		_maxWidthLabel = std::max(_maxWidthLabel, int(ll));

		if(l != "" && v != l)
			_hasShadows = true;
	}
}

void Column::_labelMapUpdates(Label * label, const std::string & previousDisplay, const std::string & previousOriginal)
{
	auto oldValDis	= std::make_pair(previousOriginal, previousDisplay);

	_labelByValDis.erase(oldValDis);
	_labelByValDis[label->origValDisplay()] = label;

	bool	valueChanged	= previousOriginal		!= label->originalValueAsString(),
			displayChanged	= previousDisplay		!= label->label(),
			previousSame	= previousOriginal		== previousDisplay,
			newSame			= label->label()		== label->originalValueAsString();

	if(valueChanged)
	{
		_labelsByValue[previousOriginal]				.erase(label);
		_labelsByValue[label->originalValueAsString()]	.insert(label);
		
		size_t prevL = stringUtils::approximateVisualLength(previousOriginal),
				newL = stringUtils::approximateVisualLength(label->originalValueAsString(true, false));
		if(newL > _maxWidthValue)
			_maxWidthValue = newL;
		else if(prevL < newL && prevL == _maxWidthValue) 
		{
			//So maybe the max width went down?
			bool sameMax = false;
			size_t newMax = 0;
			
			for(const auto & valueLabel : _labelsByValue)
			{
				size_t l = stringUtils::approximateVisualLength(valueLabel.first);
				
				newMax = std::max(newMax, l);
				
				if(l >= _maxWidthValue)
				{
					sameMax = true;
					break;
				}
			}
			
			if(!sameMax)
				_maxWidthValue = newMax;
		}
	}

	if(displayChanged)
	{
		_labelsByDisplay[previousDisplay]	.erase(label);
		_labelsByDisplay[label->label()]	.insert(label);
		
		size_t prevL = stringUtils::approximateVisualLength(previousDisplay),
				newL = stringUtils::approximateVisualLength(label->label());

		if(newL > _maxWidthLabel)
			_maxWidthLabel = newL;
		else if(prevL < newL && prevL == _maxWidthLabel) 
		{
			//So maybe the max width went down?
			bool sameMax = false;
			size_t newMax = 0;
			
			for(const auto & displayLabel : _labelsByDisplay)
			{
				size_t l = stringUtils::approximateVisualLength(displayLabel.first);
				
				newMax = std::max(newMax, l);
				
				if(l >= _maxWidthLabel)
				{
					sameMax = true;
					break;
				}
			}
			
			if(!sameMax)
				_maxWidthLabel = newMax;
		}
	}
	
	if(!newSame)
		_hasShadows = true;
	
	if(!previousSame && newSame && _hasShadows)
	{
		//Do we still have shadows now?
		_hasShadows = false;
		for(const Label * label : _labels)
			if(label->label() != label->originalValueAsString())
			{
				_hasShadows = true;
				break;
			}
	}
	label->rememberCurrentOrigValDisplay();

}

void Column::labelsHandleAutoSort(bool doDbUpdateEtc)
{
	if(_autoSortByValue)
		labelsOrderByValue(doDbUpdateEtc);
	else if(!batchedLabelDepth() && doDbUpdateEtc)
		_dbUpdateLabelOrder();
}

void Column::labelDisplayChanged(Label *label, const std::string & previousDisplay)
{
	auto origValStr = label->originalValueAsString();
	auto oldValDis  = std::make_pair(origValStr, Label::processLabel(previousDisplay, origValStr));
	bool merged		= _labelByValDis.count(label->origValDisplay()) != 0;
	
	if(merged)
		labelsMergeDuplicateInto(label);
	

	//Make sure it was registered before:
	assert(_labelByValDis[oldValDis] == label);
	//And that its new location is free:
	assert(_labelByValDis.count(label->origValDisplay()) == 0 || _labelByValDis.at(label->origValDisplay()) == label);

	_labelMapUpdates(label, previousDisplay, label->originalValueAsString());
	
	if(merged)
		_dbUpdateLabelOrder();

	nonFilteredCountersReset();
}

void Column::labelValDisplayChanged(Label *label, const std::string &previousDisplay, const Json::Value &previousOriginal)
{
	auto	oldOrigValS	= Label::originalValueAsString(this, previousOriginal);
	auto	oldValDis	= std::make_pair(oldOrigValS, Label::processLabel(previousDisplay, oldOrigValS)),
			newValDis	= std::make_pair(label->originalValueAsString(), label->label());
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
	
	_labelMapUpdates(label, previousDisplay, oldOrigValS);

	if(merged)
		_dbUpdateLabelOrder();

	nonFilteredCountersReset();
	
	dbUpdateValues();
}

Label * Column::labelByRow(int row) const
{
	if (row < rowCount() && _ints[row] != EmptyValues::missingValueInteger)
		return labelByIntsId(_ints[row]);

	return nullptr;
}


bool Column::setStringValue(size_t row, const std::string & userEntered, const std::string & labelButOnlyFromSpreadsheetPaste, bool writeToDB)
{
    JASPTIMER_SCOPE(Column::setStringValue);
    
	if(userEntered == "" && labelButOnlyFromSpreadsheetPaste == "")
	{
		const auto	cur = type() == columnType::scale ?  getValue(row, false, false) : getLabel(row, false, false);

		if(_hasLabels)
		{
			Label	*	label = labelByRow(row);

			if(label && label->isEmptyValue())	return false;
			if (cur == "")						return false;
			else								return setValue(row, EmptyValues::missingValueInteger, writeToDB);
		}
		else
		{
			if (cur == "")
				return false;

			bool aChange = setValue(row, EmptyValues::missingValueDouble, "", writeToDB);
			_handleWidthChangeWithoutLabels();
			return aChange;

		}
	}

	double		newDoubleToSet	= EmptyValues::missingValueDouble;
	bool		itsADouble		= ColumnUtils::getDoubleValue(userEntered, newDoubleToSet),
				itsMissingVal	= isEmptyValue(userEntered);	
	bool		nothingThereYet	= _hasLabels	?  std::none_of(_ints.begin(), _ints.end(), [&](int i)						{ return !(i == Label::NO_LABEL || i == EmptyValues::missingValueInteger || labelByIntsId(i)->isEmptyValue()); })
												:  std::none_of(_dbls.begin(), _dbls.end(), [&](double d)					{ return !(std::isnan(d) || isEmptyValue(d)); })
												&& std::none_of(_strs.begin(), _strs.end(), [&](const std::string & str)	{ return !str.empty(); });
	
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

	if(_hasLabels)
		return setValue(row, userEntered,		labelButOnlyFromSpreadsheetPaste,	writeToDB);
	else
	{
		bool aChange = setValue(row, newDoubleToSet,	itsADouble ? "" : userEntered,		writeToDB);
		_handleWidthChangeWithoutLabels();
		return aChange;
	}
}


bool Column::setValue(size_t row, std::string value, const std::string & label, bool writeToDB, bool useLocale)
{
	JASPTIMER_SCOPE(Column::setValue(stringstring));
    
	//If value != "" and label == "" that means we got copy pasted stuff in the viewer, and we just dont have labels, but we can treat it like we are editing.
	//if both are "" we just want to clear the cell
	//the assumption is that this is not direct user-input, but internal jasp stuff.
	if(value == "" && label == "")
		return setValue(row, EmptyValues::missingValueInteger, writeToDB);
	
	double	newDoubleToSet	= EmptyValues::missingValueDouble;
	bool	itsADouble		= ColumnUtils::getDoubleValue(value, newDoubleToSet, useLocale);
	bool	labelIsValue	= value == label,
			justAValue		= label == "";			///< To help us handle updates from synchronisation from csv (users might have added different label-texts

	if(itsADouble)
		value = ColumnUtils::doubleToString(newDoubleToSet);
	
	if(!_hasLabels)
	{
		assert(labelIsValue);
		
		return setValue(row, newDoubleToSet, !itsADouble ? value : "");
	}


	Label	* newLabel		= justAValue ? labelByValue(value) : labelByValueAndDisplay(value, label);

	if(!newLabel)
		newLabel = labelByIntsId(labelsAdd((justAValue || labelIsValue) ? value : label, "", itsADouble ? Json::Value(newDoubleToSet) : value));

	return setValue(row, newLabel->intsId(), writeToDB);
}

bool Column::setValue(size_t row, int valueInt, bool writeToDB)
{
	assert(_hasLabels);
	
	
	if(row >= _ints.size())
		return false;
	
	bool changed = _ints[row] != valueInt;
	
	_ints[row] = valueInt;
	
	if(writeToDB && !_data->writeBatchedToDB())
	{
		db().columnSetValue(_id, row, valueInt);
		incRevision();
	}
	
	return changed;
}

bool Column::setValue(size_t row, double valueDbl,  const std::string & valueStr, bool writeToDB)
{
	assert(!_hasLabels);
	
	if(row >= _dbls.size())
		return false;
	
	bool changed = _dbls[row] != valueDbl || _strs[row] != valueStr;
	
	_dbls[row] = valueDbl;
	_strs[row] = valueStr;
	
	if(writeToDB && !_data->writeBatchedToDB())
	{
		db().columnSetValue(_id, row, valueDbl, valueStr);
		incRevision();
	}
	
	return changed;
}

void Column::rowInsertEmptyVal(size_t row)
{
	if(_hasLabels)
		_ints.insert(_ints.begin() + row, EmptyValues::missingValueInteger);
	else
	{
		_dbls.insert(_dbls.begin() + row, EmptyValues::missingValueDouble);
		_strs.insert(_strs.begin() + row, "");
	}
}

void Column::rowDelete(size_t row)
{
	if(_hasLabels)
		_ints.erase(_ints.begin() + row);
	else
	{
		_dbls.erase(_dbls.begin() + row);
		_strs.erase(_strs.begin() + row);
	}
	
	nonFilteredCountersReset();
}

void Column::setRowCount(size_t rows)
{
	if(_hasLabels)
	{
		_ints.resize(rows, EmptyValues::missingValueInteger);
		_dbls.clear();
		_strs.clear();
	}
	else
	{
		_dbls.resize(rows, EmptyValues::missingValueDouble);
		_strs.resize(rows,	"");
		_ints.clear();
	}
	
	nonFilteredCountersReset();
}

Label *Column::labelByIntsId(int value) const
{
	assert(_hasLabels);
	
	return value != Label::NO_LABEL && value != EmptyValues::missingValueInteger && _labelByIntsIdMap.count(value) ? _labelByIntsIdMap.at(value) : nullptr;
}

Label * Column::labelByDisplay(const std::string & display) const
{
	assert(_hasLabels);
	
	Labelset labels		= labelsByDisplay(display);
	return labels.size() == 0 ? nullptr : *labels.begin();	
}

Labelset Column::labelsByDisplay(const std::string & display) const
{
	assert(_hasLabels);
	
	return _labelsByDisplay.count(display) ? _labelsByDisplay.at(display) : Labelset{};
}

Label * Column::labelByValue(const std::string & value) const
{
	assert(_hasLabels);
	
	Labelset labels		= labelsByValue(value);
	return labels.size() == 0 ? nullptr : *labels.begin();
}

Labelset Column::labelsByValue(const std::string & value) const
{
	assert(_hasLabels);
	
	JASPTIMER_SCOPE(Column::labelsByValue);
	return _labelsByValue.count(value) ? _labelsByValue.at(value) : Labelset{};
}

Label * Column::labelByValueAndDisplay(const std::string &value, const std::string &labelText) const
{
	JASPTIMER_SCOPE(Column::labelsByValueAndDisplay);
	
	assert(_hasLabels);

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
	}
}

bool Column::labelsRemoveOrphans()
{
	JASPTIMER_SCOPE(Column::labelsRemoveOrphans);

	intset idsNotUsed;
	
	for(size_t labelIndex=0; labelIndex < _labels.size(); labelIndex++)
		if(!_labels[labelIndex]->userAdded())
			idsNotUsed.insert(_labels[labelIndex]->intsId());
	
	for(int anInt : _ints)
		idsNotUsed.erase(anInt);
	
	labelsRemoveByIntsId(idsNotUsed);
	
	return idsNotUsed.size();
}



std::set<size_t> Column::labelsMoveRows(std::vector<size_t> rows, bool up)
{
	JASPTIMER_SCOPE(Column::labelsMoveRows);
	
	int mod = up ? -1 : 1;

	std::sort(rows.begin(), rows.end(), [&](const auto & l, const auto & r) { return up ? l < r : r < l; });

    std::vector<Label*> new_labels;

    // Put the empty labels at the end of the vector
    for (Label* label : _labels)
    {
        if (!label->isEmptyValue())
            new_labels.push_back(label);
    }
    for (Label* label : _labels)
    {
        if (label->isEmptyValue())
            new_labels.push_back(label);
    }

	for (size_t row : rows)
		if(int(row) + mod < 0 || int(row) + mod >= int(labelsNonEmptyCount()))
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
	JASPTIMER_SCOPE(Column::labelsReverse);
	
	std::reverse(_labels.begin(), _labels.end());
	_dbUpdateLabelOrder();
}

void Column::labelsOrderByValue(bool doDbUpdateEtc)
{
	JASPTIMER_SCOPE(Column::labelsOrderByValue);

	static double dummy;

	doublevec				asc			= valuesNumericOrdered();
	auto					alpha		= valuesAlphabeticalOffsets();
	size_t					ascMax		= asc.size();
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
	
	if(doDbUpdateEtc)
		_dbUpdateLabelOrder(false);
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
	
	doublevec	asc = valuesNumericOrdered(),
				dsc	= asc;
	
	std::reverse(dsc.begin(), dsc.end());
	
	dbldblmap flipIt;
	
	for(size_t i=0; i<asc.size(); i++)
		flipIt[asc[i]] = dsc[i];
	
	beginBatchedLabelsDB();
	//and now to write them back into the data
	for(Label * label : _labels)
	{
		double	aValue	= EmptyValues::missingValueDouble;
		
		if(label->originalValue().isDouble())
			aValue = label->originalValue().asDouble();
		else 
			ColumnUtils::getDoubleValue(label->originalValueAsString(), aValue);
		
		if(!std::isnan(aValue)) //not isEmptyValue because we want to use the output to rewrite the data again
		{
			std::string oldLabel = label->label();
			label->_setOriginalValue(flipIt[aValue]); //Dont trigger the mapping stuff!
			label->_label = Label::processLabel(oldLabel, label->originalValueAsString());
		}
	}
		
	dbUpdateValues();
	
	labelsHandleAutoSort(false);
	_resetLabelValueMap();
	nonFilteredCountersReset(false); //dont update indexes because its done in _dbUpdateLabelOrder()
	endBatchedLabelsDB();

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

void Column::incRevision()
{
	assert(_id != -1);

	if(!_data->writeBatchedToDB())
	{
		_revision = db().columnIncRevision(_id);
		checkForChanges();
	}
	else
		_data->batchColumnHadChange(this);
}

bool Column::checkForUpdates()
{
	assert(_id != -1);

	if(_revision >= db().columnGetRevision(_id))
		return false;

	dbLoad();
	return true;
}


void Column::addLabelManually(std::string value, std::string label)
{
	assert(_hasLabels);
	
	int labelIntsId = labelsAdd(label, value);
	
	labelByIntsId(labelIntsId)->setUserAdded(true);
	
	if(dropLevels() == dropLevelsType::noChoice) //No choice was made yet, but the user added a label, so I guess they want all labels
		setDropLevels(dropLevelsType::keep);
	
	labelsHandleAutoSort();
	incRevision();
}

void Column::deleteLabelManually(int labelIndex)
{
	assert(_hasLabels);
	
	labelsRemove(labelIndex);
	incRevision();
}


bool Column::isColumnDifferentFromStringLookUps(const std::string & title, size_t rows,	const std::function<std::string(size_t)> valueLookup, const std::function<std::string(size_t)> labelLookup, const stringset & strEmptyVals) const 
{
	if(!(title == _title && strEmptyVals == emptyValues()->emptyStrings() || rows != rowCount()))
			return true;
			
			
	for(size_t r=0; r<rowCount(); r++)
	{
		thread_local std::string impoVal, dataVal, impoLab, dataLab;

		impoVal	= valueLookup(r);
		dataVal	= getValue(r, false, true);
		impoLab	= labelLookup(r);
		dataLab = getLabel(r, false, true);

		if(impoVal != dataVal || (impoLab != "" && impoLab != dataLab))
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
	json["rCode"]			= _rCode;
	json["analysisId"]		= _analysisId;
	json["invalidated"]		= _invalidated;
	json["constructorJson"] = _constructorJson;
	json["autoSortByValue"] = _autoSortByValue;
	json["description"]		= _description;
	json["hasLabels"]		= _hasLabels;
	json["codeType"]		= int(_codeType);
	json["error"]			= _error;
	json["type"]			= int(_type);

	Json::Value jsonDbls(Json::arrayValue);
	for (double dbl : _dbls)
		jsonDbls.append(dbl);

	Json::Value jsonInts(Json::arrayValue);
	for (int i : _ints)
		jsonInts.append(i);
	
	Json::Value jsonStrs(Json::arrayValue);
	for (const std::string & str : _strs)
		jsonStrs.append(str);

	json["customEmptyValues"]	= _emptyValues->toJson();

	json["labels"]				= serializeLabels();
	json["strs"]				= jsonStrs;
	json["dbls"]				= jsonDbls;
	json["ints"]				= jsonInts;

	return json;
}

Json::Value	Column::serializeLabels(bool forCompare) const
{
	Json::Value jsonLabels(Json::arrayValue);
	for (const Label* label : _labels)
		jsonLabels.append(label->serialize(forCompare));
	
	return jsonLabels;
}

Json::Value Column::jsonForCompare() const
{
	Json::Value json(Json::objectValue);

	json["name"]				= _name;
	json["title"]				= _title;
	json["rCode"]				= _rCode;
	//json["analysisId"]			= _analysisId;
	//json["invalidated"]			= _invalidated;
	json["constructorJson"]		= _constructorJson;
	json["autoSortByValue"]		= _autoSortByValue;
	json["description"]			= _description;
	json["codeType"]			= computedColumnTypeToString(_codeType);
	//json["error"]				= _error;
	json["type"]				= columnTypeToString(_type);
	json["customEmptyValues"]	= _emptyValues->toJson();
	json["labels"]				= serializeLabels(true);
	json["data"]				= Json::arrayValue;

	for(int i=0; i<rowCount(); i++)
	{
		std::string label		= getLabel(i, true),
					value		= getValue(i, true);
				
		Json::Value row			= label == value ? Json::Value(label) : Json::objectValue;
		if(row.type() == Json::objectValue)
		{
			row["l"] = label;
			row["v"] = value;
		}

		json["data"].append(row);
	}
	
	json["nonEmptyLabels"]		= Json::arrayValue;
	for(const std::string & nonEmptyLabel : nonEmptyLevelsStrings())
		json["nonEmptyLabels"].append(nonEmptyLabel);

	return json;
}

void Column::deserializeLabelsForCopy(const Json::Value & labels)
{
	labelsClear();
	beginBatchedLabelsDB();

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
 	nonFilteredCountersReset();
	
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
	
	/* The following is already implied by endBatchedLabelsDB because it deletes all labels first anyway (There are some issues when an operation changed _labels though, in that case it might be better to deserialize the column!)
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
	_hasLabels			= json["hasLabels"].asBool();
	_analysisId			= json["analysisId"].asInt();
	_constructorJson	= json["constructorJson"];
	_autoSortByValue	= json["autoSortByValue"].asBool();

	db().columnSetComputedInfo(_id, _analysisId, _invalidated, _codeType, _rCode, _error, constructorJsonStr(), _computeFilter);
	
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
	
	i=0;
	_strs.resize(json["strs"].size());
	for (const Json::Value& strJson : json["strs"])
		_strs[i++] = strJson.asString();
	
	assert(_strs.size() == _dbls.size());
	
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

size_t Column::getMaximumWidthInCharactersIncludingShadow()
{
	if(!_hasShadows)
		return getMaximumWidthInCharacters(true, false);
	
	return getMaximumWidthInCharacters(true, false) + getMaximumWidthInCharacters(true, true, 0);
}


size_t Column::getMaximumWidthInCharacters(bool fancyEmptyValue, bool valuesPlease, size_t	extraPad)
{
	int		&	_maxWidth = valuesPlease ? _maxWidthValue : _maxWidthLabel;
	
	std::string takeWidth;
	
	if(_maxWidth < 0)
		for(Label * label : labels())
			if(!label->isEmptyValue())
			{
				takeWidth	= !valuesPlease ? label->label() : label->originalValueAsString(fancyEmptyValue, false);
				_maxWidth	= std::max(_maxWidth, int(stringUtils::approximateVisualLength(takeWidth)));
			}
	
	
	return size_t(_maxWidth) + extraPad;
}

stringvec Column::previewTransform(columnType transformType)
{
	const int showThisMany = 16;
	stringvec out;
	
	out.push_back(std::to_string(labelsNonEmptyCount()));
	out.push_back(std::to_string(nonFilteredNumericsCount()));
	
	{
		std::stringstream someValues;
		
		
		for(int count = 0; count < _ints.size() && count < showThisMany; count++)
			someValues << (count > 0 ? ", " : "") << (transformType == columnType::scale ? getValue(count, true, false, true, transformType) : '"' + getLabel(count, true) + '"');
		
		if(_ints.size() > showThisMany)
			someValues << ", ...";
		
		out.push_back(someValues.str());
	}
	
	{
		std::stringstream someImplicitEmptyValues;
		
		if(transformType == columnType::scale && labelsNonEmptyCount() > nonFilteredNumericsCount())
		{
			int count = 0;
			
			for(Label * label : _labels)
				if(!label->isEmptyValue() && std::isnan(label->originalValueAsDouble()))
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

bool Column::initFromLookups(const std::string & newName, size_t rows, const std::function<std::string(size_t)> valueLookup, const std::function<std::string(size_t)> labelLookup, const std::string & title, columnType desiredType, const stringset & emptyValues, int threshold, bool orderLabelsByValue)
{
									setHasCustomEmptyValues(emptyValues.size());
									setCustomEmptyValues(emptyValues);
									setName(newName);
									setTitle(title);
									beginBatchedLabelsDB();
	
	bool		anyChanges		=	title != Column::title() || newName != name();
	columnType	prevType		=	type(),
				suggestedType	=	setValues(rows, valueLookup, labelLookup,	threshold, &anyChanges, true, !_hasLabels);  //If less unique integers than the thresholdScale then we think it must be ordinal: https://github.com/jasp-stats/INTERNAL-jasp/issues/270
									setType(type() != columnType::unknown ? type() : desiredType == columnType::unknown ? suggestedType : desiredType);

									
	if((suggestedType == columnType::ordinal || suggestedType == columnType::nominal) && suggestedType == type() && !_hasLabels)
	{
		//There is a scenario where there might be millions of rows of strings, maybe dont store those as labels!
		stringset nonNumerics;
		const int maxNonNumeric = threshold > 0 ? threshold : 10;
		
		for(const std::string & str : _strs)
		{
			nonNumerics.insert(str);
			if(nonNumerics.size() > maxNonNumeric)
				break;
		}
		
		if(nonNumerics.size() < maxNonNumeric)
			noLabelsToLabels();
	}
		
									
	if(orderLabelsByValue)			
		labelsOrderByValue();
	
	endBatchedLabelsDB();

	return anyChanges || type() != prevType;
}



void Column::setHasLabels(bool haveLabels)
{
	if(haveLabels == _hasLabels)
		return;
	
	
	if(!_hasLabels) noLabelsToLabels();
	else			labelsToNoLabels();
}

void Column::labelsToNoLabels(bool signalOthers)
{
	const auto size = _ints.size();
	
	if(signalOthers)
		db().transactionWriteBegin();
	
	_strs.clear();
	_strs.reserve(size);
	
	_dbls.clear();
	_dbls.reserve(size);

	_maxWidthLabel	= 0;
	_maxWidthValue	= 0;
	
	for(int _int : _ints)
	{
		thread_local std::string	str;
		thread_local double			dbl;
		thread_local Label	*		label;
		
		label	= labelByIntsId(_int);
		str		= label ? label->_label		: "";
		dbl		= label ? label->_dblValue	: EmptyValues::missingValueDouble;
		
		_strs.push_back(str);
		_dbls.push_back(dbl);

		_maxWidthValue = std::max(_maxWidthValue, int(stringUtils::approximateVisualLength(str.empty() ? doubleToDisplayString(dbl) : str)));
	}

	_maxWidthLabel = _maxWidthValue;
	
	labelsClear(false);
	
	_ints.clear();
	
	_hasLabels		= false;
	_hasShadows		= false;
	
	if(signalOthers)
	{
		db().columnSetValues(_id, _dbls, _strs);
		db().columnSetHasLabels(_id, _hasLabels);
		db().transactionWriteEnd();
		
		incRevision();
	}
}

void Column::noLabelsToLabels()
{
	const auto size = _dbls.size();
	
	db().transactionWriteBegin();
	beginBatchedLabelsDB();
	_hasLabels	= true;
	_hasShadows = false;
	
	_ints.clear();
	_ints.reserve(size);
	
	for(size_t row=0; row<size; row++)
		_ints.push_back(_strs[row].empty() ? labelsAdd(_dbls[row]) : labelsAdd(_strs[row]));
	
	_dbls.clear();
	_strs.clear();
	
	db().columnSetValues(_id, _ints);
	db().columnSetHasLabels(_id, _hasLabels);
	
	labelsHandleAutoSort(false);

	endBatchedLabelsDB();
	
	db().transactionWriteEnd();
	
	incRevision();
}
