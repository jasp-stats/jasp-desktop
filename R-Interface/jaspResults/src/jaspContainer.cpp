#include "jaspContainer.h"

void jaspContainer::insert(std::string field, Rcpp::RObject value)
{
	if(value.isNULL())
	{
		if(_data.count(field) > 0)
			_data.erase(field); //deletion will be taken care of by jaspObject::destroyAllAllocatedObjects()

		return;
	}

	jaspObject * obj = nullptr;


	if(Rcpp::is<jaspObject_Interface>(value))			obj = Rcpp::as<jaspObject_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspContainer_Interface>(value))	obj = Rcpp::as<jaspContainer_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspQmlSource_Interface>(value))	obj = Rcpp::as<jaspQmlSource_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspColumn_Interface>(value))		obj = Rcpp::as<jaspColumn_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspTable_Interface>(value))		obj = Rcpp::as<jaspTable_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspState_Interface>(value))		obj = Rcpp::as<jaspState_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspPlot_Interface>(value))		obj = Rcpp::as<jaspPlot_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspHtml_Interface>(value))		obj = Rcpp::as<jaspHtml_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<Rcpp::List>(value))				obj = (jaspObject*)(jaspContainerFromRcppList(Rcpp::as<Rcpp::List>(value)));
	else												obj = new jaspJson(value);

#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "something {"<<obj->objectTitleString()<<"} added to jaspContainer "<<objectTitleString()<<" on field "<<field<<std::endl<<std::flush;
#endif

	_data[field] = obj; //If we overwrite anything: deletion will be taken care of by jaspObject::destroyAllAllocatedObjects()

	obj->setName(field);

	if (_error)
		obj->setError();

	if(_data_order.count(field) == 0) //this way we can keep the order after removing the original object due to changes/options-changing or whatever because the order will stay the same
		_data_order[field] = _order_increment++;

	addChild(obj);

	if (connectedToJaspResults())
		renderPlotsOfChildren();

	//jaspResults doesn't have any parents
	if(getType() == jaspObjectType::results)	childrenUpdatedCallback(true);
	else										notifyParentOfChanges();
}

jaspContainer * jaspContainer::jaspContainerFromRcppList(Rcpp::List convertThis)
{
	std::vector<std::string> colNamesVec = extractElementOrColumnNames(convertThis);

	if(convertThis.size() > colNamesVec.size())
		Rf_error("If you add a list() to jaspResults or a jaspContainer each element should be named!");

	jaspContainer * newContainer = new jaspContainer();

	for(int i=0; i<convertThis.size(); i++)
		if(colNamesVec[i] == "title")
			newContainer->_title = Rcpp::as<std::string>(convertThis[i]);
		else
			newContainer->insert(colNamesVec[i], convertThis[i]);

	return newContainer;
}

Rcpp::RObject jaspContainer::at(std::string field)
{
	if(_data.count(field) == 0)
		return R_NilValue;

	jaspObject * ref = _data[field];
	return wrapJaspObject(ref);
}

Rcpp::RObject jaspContainer::wrapJaspObject(jaspObject * ref)
{
	switch(ref->getType())
	{
	case jaspObjectType::container:	return Rcpp::wrap(jaspContainer_Interface(ref));
	case jaspObjectType::qmlSource:	return Rcpp::wrap(jaspQmlSource_Interface(ref));
	case jaspObjectType::column:	return Rcpp::wrap(jaspColumn_Interface(ref));
	case jaspObjectType::table:		return Rcpp::wrap(jaspTable_Interface(ref));
	case jaspObjectType::state:		return Rcpp::wrap(jaspState_Interface(ref));
	case jaspObjectType::html:		return Rcpp::wrap(jaspHtml_Interface(ref));
	case jaspObjectType::plot:		return Rcpp::wrap(jaspPlot_Interface(ref));
	case jaspObjectType::json:		return Rcpp::wrap(((jaspJson*)ref)->jsonToPrefixedStrings());
	default:						return R_NilValue;
	}
}

std::string jaspContainer::dataToString(std::string prefix) const
{
	std::stringstream out;

	for(auto key : getSortedDataFields())
		out << prefix << "\"" << key << "\":\n" << _data.at(key)->toString(prefix + "\t") << "\n";

	return out.str();
}

std::string jaspContainer::toHtml()
{
	std::stringstream out;

	out		<< "<div class=\"jaspHtml\">" "\n"
			<< htmlTitle() << "\n"
			<< "<ul>";

	for(auto key : getSortedDataFields())
		out << "<li><p><b>" << key << "</b></p>" << _data[key]->toHtml() << "</li>\n";

	out << "</ul>" "\n" "</div>" "\n";

	return out.str();
}

std::vector<std::pair<double, std::string>> jaspContainer::getSortedDataFieldsSortVector() const
{
	std::vector<std::pair<double, std::string>> sortvec;

	int maxOrder = 0;
	for(auto ord : _data_order)
		maxOrder = std::max(ord.second, maxOrder);

	double orderDivider = 1.0 / double(maxOrder + 1);

	for(auto ord : _data_order)
		if(_data.count(ord.first) > 0) //if it was removed from the data it should probably not be in the ordered list with its field
		{
			jaspObject * obj = _data.at(ord.first);
			double sortVal = double(obj->_position) + (double(ord.second) * orderDivider);

			sortvec.push_back(std::make_pair<double, std::string>(double(sortVal), std::string(ord.first)));
		}

	return sortvec;
};

std::vector<std::string> jaspContainer::convertSortedDataFieldsToStringVector(std::vector<std::pair<double, std::string>> sortvec, bool removeDuplicates)
{
	std::set<std::string>		dup;
	std::vector<std::string>	out;

	for(auto sortval : sortvec)
	{
		if(!removeDuplicates || dup.count(sortval.second) == 0)
			out.push_back(sortval.second);

		if(removeDuplicates)
			dup.insert(sortval.second);
	}

	return out;
}

std::vector<std::string> jaspContainer::getSortedDataFields() const
{
	std::vector<std::pair<double, std::string>> sortvec = getSortedDataFieldsSortVector();
	std::sort(sortvec.begin(), sortvec.end());
	return convertSortedDataFieldsToStringVector(sortvec);
}

bool jaspContainer::jaspObjectComesFromOldResults(std::string fieldName, jaspContainer * oldResult) const
{
	return !(oldResult == nullptr || _data.count(fieldName) > 0);
}

jaspObject * jaspContainer::findObjectWithNestedNameVector(const std::vector<std::string>& uniqueNames, const size_t position)
{
	if (_data.count(uniqueNames[position]) == 0)
		return nullptr;

	jaspObject * ptr = _data[uniqueNames[position]];
	if (uniqueNames.size() == position + 1)
		return ptr;

	if (ptr->getType() == jaspObjectType::container)
		return static_cast<jaspContainer *>(ptr)->findObjectWithNestedNameVector(uniqueNames, position + 1);

	return ptr;

}

jaspObject * jaspContainer::findObjectWithUniqueNestedName(const std::string &uniqueNestedName)
{

	if (getUniqueNestedName() == uniqueNestedName)
		return this;

	for (auto & d: _data)
	{
		if (d.second->getUniqueNestedName() == uniqueNestedName)
			return d.second;

		if (d.second->getType() == jaspObjectType::container)
		{
			jaspObject * ptr = static_cast<jaspContainer *>(d.second)->findObjectWithUniqueNestedName(uniqueNestedName);
			if (ptr)
				return ptr;
		}
	}

	return nullptr;

}

jaspObject * jaspContainer::getJaspObjectNewOrOld(std::string fieldName, jaspContainer * oldResult) const
{
	return jaspObjectComesFromOldResults(fieldName, oldResult) ? oldResult->getJaspObjectFromData(fieldName) :  _data.at(fieldName);
}

jaspObject * jaspContainer::getJaspObjectFromData(std::string fieldName) const
{
	return _data.count(fieldName) > 0 ? _data.at(fieldName) : nullptr;
}

std::vector<std::string> jaspContainer::getSortedDataFieldsWithOld(jaspContainer * oldResult) const
{
	auto newsortvec = getSortedDataFieldsSortVector();
	auto oldsortvec = oldResult ? oldResult->getSortedDataFieldsSortVector() : std::vector<std::pair<double, std::string>>();

	//Move old fields into the new one https://stackoverflow.com/a/21972296
	newsortvec.insert( newsortvec.end(),	 std::make_move_iterator(oldsortvec.begin()),	 std::make_move_iterator(oldsortvec.end())  );
	oldsortvec.clear();

	std::sort(newsortvec.begin(), newsortvec.end());

	return convertSortedDataFieldsToStringVector(newsortvec, true);
}

std::string jaspContainer::getCommonDenominatorMetaType() const
{
	std::string comDenom = "";

	for(auto keyval: _data)
	{
		std::string currentType = keyval.second->metaEntry()["type"].asString();
		if(comDenom == "")
			comDenom = currentType;
		else if(comDenom != currentType) //at least two kinds of types in here
			return "various";
	}

	return comDenom;
}

Json::Value jaspContainer::metaEntry(jaspObject * oldResult) const
{
	Json::Value submeta = Json::arrayValue,
				meta	= constructMetaEntry("collection", getCommonDenominatorMetaType());

	jaspContainer * oldContainer = dynamic_cast<jaspContainer*>(oldResult); //dynamic_cast returns nullptr if not right type

	std::vector<std::string> orderedDataFields = getSortedDataFieldsWithOld(oldContainer);

	for(std::string field: orderedDataFields)
	{
		jaspObject *	obj			= getJaspObjectNewOrOld(field, oldContainer);
		bool			objIsOld	= jaspObjectComesFromOldResults(field, oldContainer);

		if(obj->shouldBePartOfResultsJson())
			submeta.append(obj->metaEntry(objIsOld || !oldContainer ? nullptr : oldContainer->getJaspObjectFromData(field)));
	}

	meta["meta"] = submeta;

	return meta;
}

Json::Value jaspContainer::dataEntry(jaspObject * oldResult, std::string & errorMsg) const
{
	Json::Value dataJson			= jaspObject::dataEntryBase();					//jaspContainer should not try to set any errorMessage on itself
	dataJson["title"]				= _title;
	dataJson["name"]				= getUniqueNestedName();
	dataJson["initCollapsed"]		= _initiallyCollapsed;
	dataJson["collection"]			= Json::objectValue;
	bool cascaded					= errorMsg != "";
	std::string cascadingMsg		= cascaded ? errorMsg : _errorMessage;	//cascading errorMessagues trumps local one
	jaspContainer * oldContainer	= dynamic_cast<jaspContainer*>(oldResult);		//dynamic_cast returns nullptr if not right type

	for(std::string field: getSortedDataFieldsWithOld(oldContainer))
	{
		jaspObject *	obj			= getJaspObjectNewOrOld(field, oldContainer);
		bool			objIsOld	= jaspObjectComesFromOldResults(field, oldContainer);

		if(obj->shouldBePartOfResultsJson())
		{
			dataJson["collection"][obj->getUniqueNestedName()] = obj->dataEntry(objIsOld || !oldContainer ? nullptr : oldContainer->getJaspObjectFromData(field), cascadingMsg);

			if(cascaded && cascadingMsg == "")
				errorMsg = "";
		}
	}

	return dataJson;
}

void jaspContainer::childFinalizedHandler(jaspObject *child)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "JASPcontainer " << title << " removes child " << child->title << " from data etc!\n" << std::flush;
#endif

	std::list<std::string> fieldsToRemove;

	for(auto keyval : _data)
		if(keyval.second == child)
			fieldsToRemove.push_back(keyval.first);

	for(auto field : fieldsToRemove)
	{
		_data.erase(field);
		_data_order.erase(field);
	}

}

void jaspContainer::letChildrenRun()
{
	for(auto keyval : _data)
	{
		jaspObject * obj = keyval.second;

		switch(obj->getType())
		{
		case jaspObjectType::container:
			static_cast<jaspContainer*>(obj)->letChildrenRun();
			break;

		case jaspObjectType::table:
			static_cast<jaspTable*>(obj)->letRun();
			break;

		case jaspObjectType::plot:
			static_cast<jaspPlot*>(obj)->letRun();
			break;

		default:
			break;
		}
	}
}


void jaspContainer::completeChildren()
{
	for(auto keyval : _data)
	{
		jaspObject * obj = keyval.second;

		switch(obj->getType())
		{
		case jaspObjectType::container:
			static_cast<jaspContainer*>(obj)->completeChildren();
			break;

		case jaspObjectType::table:
			static_cast<jaspTable*>(obj)->complete();
			break;

		case jaspObjectType::plot:
			static_cast<jaspPlot*>(obj)->complete();
			break;

		default:
			break;
		}
	}
}


bool jaspContainer::containsNonContainer()
{
	for(const auto & keyval : _data)
	{
		jaspObject * obj = keyval.second;

		switch(obj->getType())
		{
		case jaspObjectType::container:
			if(static_cast<jaspContainer*>(obj)->containsNonContainer())
				return true;
			break;

		default:
				return true;
		}
	}

	return false;
}

bool jaspContainer::canShowErrorMessage() const
{
	for(const auto & keyval : _data)
		if(keyval.second->canShowErrorMessage())
			return true;

	return false;
}

Json::Value jaspContainer::convertToJSON() const
{
	Json::Value obj			= jaspObject::convertToJSON();
	obj["initCollapsed"]	= _initiallyCollapsed;
	obj["data"]				= Json::objectValue;
	obj["data_order"]		= Json::objectValue;
	obj["order_increment"]	= _order_increment;

	for(const auto & d : _data)
		obj["data"][d.first] = d.second->convertToJSON();

	for(const auto & d : _data_order)
		if(_data.count(d.first) > 0) //no need to keep remembering lost items positions
			obj["data_order"][d.first] = d.second;

	return obj;
}

void jaspContainer::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_order_increment = in.get("order_increment", -1).asInt();

	_data.clear();
	_data_order.clear();

	Json::Value dataIn = in.get("data", Json::objectValue);

	for(auto & dataEntry : dataIn.getMemberNames())
	{
		_data[dataEntry] = jaspObject::convertFromJSON(dataIn[dataEntry]);
		addChild(_data[dataEntry]);
	}

	Json::Value dataOrderIn = in.get("data_order", Json::objectValue);

	for(auto & dataOrderEntry : dataOrderIn.getMemberNames())
		_data_order[dataOrderEntry] = dataOrderIn.get(dataOrderEntry, -1).asInt();

	_initiallyCollapsed = in.get("initCollapsed", false).asBool();
}

void jaspContainer::checkDependenciesChildren(Json::Value currentOptions)
{
	std::vector<std::string> removeThese;
	for(auto & d : _data)
		if(!d.second->checkDependencies(currentOptions))
			removeThese.push_back(d.first);

	for(auto & removeThis : removeThese)
	{
		delete _data[removeThis];
		_data.erase(removeThis);
	}
}

void jaspContainer::setError()
{
	_error = true;
	for(auto & d : _data)
		d.second->setError();
}

void jaspContainer::setError(std::string message)
{
	_errorMessage = message;
	setError();
}

void jaspContainer::renderPlotsOfChildren()
{
	for(auto & child : _data)
		switch (child.second->getType())
		{
		case jaspObjectType::container:
			static_cast<jaspContainer*>(child.second)->renderPlotsOfChildren();
			break;

		case jaspObjectType::plot:
			static_cast<jaspPlot*>(child.second)->renderPlot();
			break;

		default:
			break;

		}
}

Rcpp::RObject jaspContainer_Interface::findObjectWithUniqueNestedName(std::string uniqueNestedName)
{
	return jaspContainer::wrapJaspObject(((jaspContainer*)myJaspObject)->findObjectWithUniqueNestedName(uniqueNestedName));
}
