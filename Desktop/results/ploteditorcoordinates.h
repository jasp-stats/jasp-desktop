#ifndef PLOTEDITORCOORDINATES_H
#define PLOTEDITORCOORDINATES_H

#include <string>
#include <vector>
#include "jsonredirect.h"

namespace PlotEditor
{

class HitRectangle
{
public:
	HitRectangle(std::string elementName, double x0, double y0, double x1, double y1) :
		_elementName(elementName), _x(x0), _y(y0), _w(x1-x0), _h(y1-y0) {}

	double				area()						const { return _w * _h; }
	const std::string & elementName()				const { return _elementName; }
	bool				amIHit(double x, double y)	const { return x >= _x && x <= _x + _w && y >= _y && y <= _y + _h; }

private:
	std::string	_elementName;
	double		_x, _y, _w, _h;
};

class Coordinates
{
public:
	Coordinates() {}

	bool	elementHit(double x, double y, std::string & elementName) const;

	void	loadCoordinates(Json::Value coordsFromEditOptions);

private:
	std::vector<HitRectangle>	_rects;
};


}

#endif // PLOTEDITORCOORDINATES_H
