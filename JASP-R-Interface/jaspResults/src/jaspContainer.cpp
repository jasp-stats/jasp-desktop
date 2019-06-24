#include "jaspContainer.h"


void jaspContainer::insert(std::string field, Rcpp::RObject value)
{
	if(value.isNULL())
	{
		if(_data.count(field) > 0)
			_data.erase(field); //deletion will be taken care of by jaspObject::destroyAllAllocatedObjects()

		return;
	}

	jaspObject * obj;


	if(Rcpp::is<jaspObject_Interface>(value))			obj = Rcpp::as<jaspObject_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspContainer_Interface>(value))	obj = Rcpp::as<jaspContainer_Interface>(value).returnMyJaspObject();
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
	if (_passErrorMessageToNextChild)
	{
		obj->setError(_errorMessage);
		_passErrorMessageToNextChild = false;
	}
	else if (_error)
	{
		obj->setError();
	}

	if(_data_order.count(field) == 0) //this way we can keep the order after removing the original object due to changes/options-changing or whatever because the order will stay the same
		_data_order[field] = _order_increment++;

	addChild(obj);
	notifyParentOfChanges();
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

	switch(ref->getType())
	{
	case jaspObjectType::json:		return Rcpp::wrap(((jaspJson*)ref)->jsonToPrefixedStrings());
	case jaspObjectType::html:		return Rcpp::wrap(jaspHtml_Interface(ref));
	case jaspObjectType::container:	return Rcpp::wrap(jaspContainer_Interface(ref));
	case jaspObjectType::column:	return Rcpp::wrap(jaspColumn_Interface(ref));
	case jaspObjectType::table:		return Rcpp::wrap(jaspTable_Interface(ref));
	case jaspObjectType::state:		return Rcpp::wrap(jaspState_Interface(ref));
	case jaspObjectType::plot:		return Rcpp::wrap(jaspPlot_Interface(ref));
	default:						return R_NilValue;
	}
}

std::string jaspContainer::dataToString(std::string prefix)
{
	std::stringstream out;

	for(auto key : getSortedDataFields())
		out << prefix << "\"" << key << "\":\n" << _data[key]->toString(prefix + "\t") << "\n";

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

std::vector<std::string> jaspContainer::getSortedDataFields()
{
	std::vector<std::pair<double, std::string>> sortvec;

	int maxOrder = 0;
	for(auto ord : _data_order)
		maxOrder = std::max(ord.second, maxOrder);

	double orderDivider = 1.0 / double(maxOrder + 1);

	for(auto ord : _data_order)
		if(_data.count(ord.first) > 0) //if it was removed from the data it should probably not be in the ordered list with its field
		{
			jaspObject * obj = _data[ord.first];
			double sortVal = double(obj->_position) + (double(ord.second) * orderDivider);

			sortvec.push_back(std::make_pair<double, std::string>(double(sortVal), std::string(ord.first)));
		}

	std::sort(sortvec.begin(), sortvec.end());

	std::vector<std::string> out;
	for(auto sortval : sortvec)
		out.push_back(sortval.second);

	return out;
}

std::string jaspContainer::getCommonDenominatorMetaType()
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

Json::Value jaspContainer::metaEntry()
{
	Json::Value submeta(Json::arrayValue), meta = constructMetaEntry("collection", getCommonDenominatorMetaType());

	std::vector<std::string> orderedDataFields = getSortedDataFields();

	for(std::string field: orderedDataFields)
		if(_data[field]->shouldBePartOfResultsJson())
			submeta.append(_data[field]->metaEntry());

	meta["meta"] = submeta;

	return meta;
}

Json::Value jaspContainer::dataEntry()
{
	Json::Value dataJson(jaspObject::dataEntry());

	dataJson["title"] = _title;
	dataJson["name"] = getUniqueNestedName();

	Json::Value collection(Json::objectValue);

	for(std::string field: getSortedDataFields())
		if(_data[field]->shouldBePartOfResultsJson())
			collection[_data[field]->getUniqueNestedName()] = _data[field]->dataEntry();

	dataJson["collection"] = collection;

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
		}
	}
}

Json::Value jaspContainer::convertToJSON()
{
	Json::Value obj			= jaspObject::convertToJSON();
	obj["data"]				= Json::objectValue;
	obj["data_order"]		= Json::objectValue;
	obj["order_increment"]	= _order_increment;

	for(auto d : _data)
		obj["data"][d.first] = d.second->convertToJSON();

	for(auto d : _data_order)
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
}

void jaspContainer::checkDependenciesChildren(Json::Value currentOptions)
{
	std::vector<std::string> removeThese;
	for(auto & d : _data)
		if(!d.second->checkDependencies(currentOptions))
			removeThese.push_back(d.first);

	for(auto & removeThis : removeThese)
		_data.erase(removeThis);
}

void jaspContainer::setError()
{
	_error = true;
	for(auto & d : _data)
		d.second->setError();
}

void jaspContainer::setError(std::string message)
{
	jaspObject::setError(message);
	if (_data.empty())
	{
		_passErrorMessageToNextChild = true;
	} 
	else
	{
		for(auto & d : _data)
			d.second->setError();
		_data.begin()->second->setError(message);
	}
}
