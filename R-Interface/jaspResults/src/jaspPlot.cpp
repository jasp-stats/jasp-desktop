#include "jaspPlot.h"
#include "jaspResults.h"


jaspPlot::~jaspPlot()
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	JASPprint("Destructor of JASPplot("+title+") is called! ");
#endif

	finalizedHandler();
}

std::string jaspPlot::dataToString(std::string prefix) const
{
	std::stringstream out;

	out <<
		prefix << "aspectRatio: "	<< _aspectRatio << "\n" <<
		prefix << "dims:        "	<< _width << "X" << _height << "\n" <<
		prefix << "error:       '"	<< _error << "': '" << _errorMessage << "'\n" <<
		prefix << "filePath:    "	<< _filePathPng << "\n" <<
		prefix << "status:      "	<< _status << "\n" ;//<<
		//prefix << "has plot:    "	<< (_plotObjSerialized.size() > 0 ? "yes" : "no") << "\n";

	return out.str();
}

Json::Value jaspPlot::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

	data["title"]		= _title;
	data["convertible"]	= true;
	data["data"]		= _filePathPng;
	data["height"]		= _height;
	data["width"]		= _width;
	data["aspectRatio"]	= _aspectRatio;
	data["status"]		= _error ? "error" : _status;
	data["revision"]	= _revision;
	data["name"]		= getUniqueNestedName();
	data["editOptions"]	= _editOptions;
	data["reasonNotEditable"] = _editOptions.get("reasonNotEditable", "unknown reason");
	data["errorType"]	= _editOptions.get("errorType", "fatalError");
	data["editable"]	= !_editOptions.isNull() && data["errorType"] == "success";

	return data;
}

void jaspPlot::initEnvName()
{
	static int counter = 0;

	_envName = "plot_" + std::to_string(counter++);
}

void jaspPlot::setPlotObject(Rcpp::RObject obj)
{
	Rcpp::List plotInfo = Rcpp::List::create(Rcpp::_["obj"] = obj, Rcpp::_["width"] = _width, Rcpp::_["height"] = _height, Rcpp::_["revision"] = _revision);
	_filePathPng = "";

	jaspResults::setObjectInEnv(_envName, plotInfo);

	if (connectedToJaspResults())
		renderPlot();

}

void jaspPlot::renderPlot()
{
	// if  a png exists the plot was already rendered
	if (_filePathPng != "")
		return;

	Rcpp::List plotInfo = Rcpp::as<Rcpp::List>(jaspResults::getObjectFromEnv(_envName));
	Rcpp::RObject obj = plotInfo["obj"];
	jaspPrint("Now rendering a plot!");

	if(!obj.isNULL())
	{

		Rcpp::List oldPlotInfo = getOldPlotInfo(plotInfo);
		//getOldPlotInfo may update height & width

		static Rcpp::Function tryToWriteImage = jaspResults::isInsideJASP() ? Rcpp::Function("tryToWriteImageJaspResults") : Rcpp::Environment::namespace_env("jaspResults")["tryToWriteImageJaspResults"];
		Rcpp::List writeResult = tryToWriteImage(Rcpp::_["width"] = _width, Rcpp::_["height"] = _height, Rcpp::_["plot"] = obj, Rcpp::_["oldPlotInfo"] = oldPlotInfo);

		// we need to overwrite plot functions with their recordedplot result
		if(Rcpp::is<Rcpp::Function>(obj) && writeResult.containsElementNamed("obj"))
			plotInfo["obj"] = writeResult["obj"];

		if(writeResult.containsElementNamed("png"))
			_filePathPng = Rcpp::as<std::string>(writeResult["png"]);

		_editOptions = Json::nullValue;

		if(writeResult.containsElementNamed("editOptions") && !Rf_isNull(writeResult["editOptions"]))
		{
			std::string editOptionsStr = Rcpp::as<std::string>(writeResult["editOptions"]);

			if(editOptionsStr != "")
			{
				_editOptions = Json::objectValue;
				Json::Reader().parse(editOptionsStr, _editOptions);
			}
		}

		if(writeResult.containsElementNamed("error"))
		{
			_error			= "Error during writeImage";
			_errorMessage	= Rcpp::as<std::string>(writeResult["error"]);
		}

		complete();
	}

	jaspResults::setObjectInEnv(_envName, plotInfo);
}

Rcpp::RObject jaspPlot::getPlotObject()
{
	Rcpp::RObject plotInfo = jaspResults::getObjectFromEnv(_envName);
	if (!plotInfo.isNULL() && Rcpp::is<Rcpp::List>(plotInfo))
	{
		
		Rcpp::List plotInfoList = Rcpp::as<Rcpp::List>(plotInfo);
		if (plotInfoList.containsElementNamed("obj"))
			return Rcpp::as<Rcpp::RObject>(plotInfoList["obj"]);
			
	}
	return R_NilValue;
}

void jaspPlot::setUserPlotChangesFromRStateObject()
{
	Rcpp::RObject plotInfo = jaspResults::getObjectFromEnv(_envName);
	if (plotInfo.isNULL() || !Rcpp::is<Rcpp::List>(plotInfo))
		return;
	
	Rcpp::List plotInfoList = Rcpp::as<Rcpp::List>(plotInfo);
	
	if (plotInfoList.containsElementNamed("width"))
		_width = Rcpp::as<int>(plotInfoList["width"]);
	
	if (plotInfoList.containsElementNamed("height"))
		_height = Rcpp::as<int>(plotInfoList["height"]);
	
	if (plotInfoList.containsElementNamed("revision"))
		_revision = Rcpp::as<int>(plotInfoList["revision"]);
}

Rcpp::List jaspPlot::getOldPlotInfo(Rcpp::List & plotInfo)
{
	std::stack<std::string> names;
	jaspContainer * oldResults = getNamesChainToJaspResults(names);

	if (oldResults == nullptr)
		return Rcpp::List();

	jaspObject * temp;
	while (!names.empty())
	{

		temp = oldResults->getJaspObjectFromData(names.top());
		// should be impossible, but who knows
		if (temp == nullptr)
			break;

		jaspPrint("found a " + temp->type() + " with name: " + names.top());
		switch(temp->getType())
		{
		case jaspObjectType::container:
			oldResults = static_cast<jaspContainer *>(temp);
			break;
		case jaspObjectType::plot:
		{
			jaspPlot * oldPlot = static_cast<jaspPlot *>(temp);
			// we could reuse the old environment, but it might get thrown away / cleaned?
			Rcpp::List oldOptions = Rcpp::as<Rcpp::List>(jaspResults::getObjectFromEnv(oldPlot->_envName));
			_width				= oldPlot->_width;
			_height				= oldPlot->_height;
			plotInfo["width"]	= _width;
			plotInfo["heitgh"]	= _height;

			if (oldPlot->_editOptions == Json::nullValue)
				return Rcpp::List();
			else
				return Rcpp::List::create(Rcpp::_["editOptions"] = Rcpp::String(oldPlot->_editOptions.toStyledString()));

		}
		default: // possible but means the R code reuses the same names for e.g., tables and plots
			return Rcpp::List();
		}
		names.pop();
	}

	// there never was a plot
	return Rcpp::List();
}

Json::Value jaspPlot::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();

	obj["aspectRatio"]			= _aspectRatio;
	obj["width"]				= _width;
	obj["height"]				= _height;
	obj["status"]				= _status;
	obj["filePathPng"]			= _filePathPng;
	obj["revision"]				= _revision;
	obj["environmentName"]		= _envName;
	obj["editOptions"]			= _editOptions;

	return obj;
}

void jaspPlot::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_aspectRatio	= in.get("aspectRatio",		0.0f).asDouble();
	_width			= in.get("width",			-1).asInt();
	_height			= in.get("height",			-1).asInt();
	_revision		= in.get("revision", 		0).asInt();
	_status			= in.get("status",			"complete").asString();
	_filePathPng	= in.get("filePathPng",		"null").asString();
	_envName		= in.get("environmentName",	_envName).asString();
	_editOptions	= in.get("editOptions",		Json::nullValue);
	
	setUserPlotChangesFromRStateObject();
	
	/*JASP_OBJECT_TIMERBEGIN
	std::string jsonPlotObjStr = in.get("plotObjSerialized", "").asString();
	_plotObjSerialized = Rcpp::Vector<RAWSXP>(jsonPlotObjStr.begin(), jsonPlotObjStr.end());
	JASP_OBJECT_TIMEREND(converting from JSON)*/
}

std::string jaspPlot::toHtml()
{
	std::stringstream out;

	out << "<div class=\"status " << _status << "\">" "\n"
		<< htmlTitle() << "\n";

	if(_error || _errorMessage != "")
	{
		out << "<p class=\"error\">\n";
		if(_error		      ) out << "error: <i>'" << _error << "'</i>";
		if(_errorMessage != "") out << (_error       ? " msg: <i>'" : "errormessage: <i>'") << _errorMessage << "'</i>";
		out << "\n</p>";
	}
	else
		out << "<img src=\"" << _filePathPng << "\" height=\"" << _height << "\" width=\"" << _width << "\" alt=\"a plot called " << _title << "\">";

	out << "</div>\n";

	return out.str();
}
