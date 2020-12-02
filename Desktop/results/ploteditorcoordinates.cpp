#include "ploteditorcoordinates.h"
#include "log.h"


namespace PlotEditor
{

bool Coordinates::elementHit(double x, double y, std::string & elementName) const
{
	const HitRectangle * found = nullptr;
	elementName = "";

	for(size_t hit=0; hit < _rects.size(); hit++)
		if(_rects[hit].amIHit(x, y))
		{
			const HitRectangle * cur = &(_rects[hit]);

			//If we haven't hit anything before then store this hit (obviously) otherwise check if this area is smaller because then it is more important.
			if(found == nullptr || found->area() > cur->area())
				found = cur;
			//Something to consider would be to add a z-value for depth and use the area check only when found->z() == cur->z()
		}

	if(found)
		elementName = found->elementName();

	return found;
}

void Coordinates::loadCoordinates(Json::Value coordsFromEditOptions)
{
	_rects.clear();

	//This is where the coordinates are loaded from json

	//Assuming the json we get is something like:
	/*
	 [
		{ "name": "plotTitle",	"x0": 1.34, "x1": 17.56, "y0": 16.57, "y1": 17.19 },
		{ "name": "yAxisTitle", "x0": 0.19, "x1": 0.69,  "y0": 1.11,  "y1": 16.57 },
		etcetera
	]
	Then we can do the following:
	*/

	auto lambdaMemberCheck = [](const Json::Value & entry, std::vector<const char *> membersToCheck)
	{
		for(const char * mem : membersToCheck)
			if(!entry.isMember(mem))
				return false;
		return true;
	};

	bool rightFormat = coordsFromEditOptions.isArray();

	if(rightFormat)
	{
		for(const Json::Value & entry : coordsFromEditOptions)
			if(!entry.isObject() || !lambdaMemberCheck(entry, {"name", "x0", "x1", "y0", "y1"}))
			{
				rightFormat = false;
				break;
			}
			else
				_rects.push_back(
					HitRectangle(
						entry["name"].asString(),
						entry["x0"].asDouble(), entry["y0"].asDouble(),
						entry["x1"].asDouble(), entry["y1"].asDouble()
					)
				);
	}
	else
		Log::log() << "coordinates for plotEditing were not in the right format!\nReceived:" << coordsFromEditOptions.toStyledString();
}

}
