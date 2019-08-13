#include "ploteditormodel.h"
#include "analysis/analyses.h"
#include "utilities/qutils.h"
#include "log.h"
#include "tempfiles.h"

PlotEditorModel::PlotEditorModel(Analyses * analyses)
	: QObject(analyses), _analyses(analyses)
{
	_xAxis = new PlotEditorAxisModel(this, true);
	_yAxis = new PlotEditorAxisModel(this, false);

	connect(_xAxis, &PlotEditorAxisModel::somethingChanged, this, &PlotEditorModel::somethingChanged);
	connect(_yAxis, &PlotEditorAxisModel::somethingChanged, this, &PlotEditorModel::somethingChanged);
}

void PlotEditorModel::showPlotEditor(int id, QString options)
{
	setVisible(false);

	_analysisId	= id;
	_analysis	= _analyses->get(id);
	_imgOptions	= Json::objectValue;

	Json::Reader().parse(fq(options), _imgOptions);

	//maybe the following checks are a bit extreme but whatever
	if(!_analysis || !_imgOptions.isMember("type") || _imgOptions["type"].type() != Json::stringValue || _imgOptions["type"] != "interactive")
		return;

	setName(	tq(			_imgOptions.get(	"name",			"").asString()));
	setData(	tq(			_imgOptions.get(	"data",			"").asString()));
	setTitle(	tq(			_imgOptions.get(	"title",		"").asString()));
	setWidth(				_imgOptions.get(	"width",		100).asInt());
	setHeight(				_imgOptions.get(	"height",		100).asInt());

	_editOptions		=	_imgOptions.get(	"editOptions",	Json::objectValue);
	Json::Value	xAxis	=	_editOptions.get(	"xAxis",		Json::objectValue),
				yAxis	=	_editOptions.get(	"yAxis",		Json::objectValue);

	_xAxis->setAxisData(xAxis);
	_yAxis->setAxisData(yAxis);

	setVisible(true);
}

void PlotEditorModel::somethingChanged() const
{
	if(!_visible) return; // We're still loading!

	Json::Value newImgOptions  = _imgOptions,
				newEditOptions = _editOptions;

	newImgOptions["data"]			= _data.toStdString();
	newImgOptions["title"]			= _title.toStdString();
	newEditOptions["xAxis"]			= _xAxis->getAxisData();
	newEditOptions["yAxis"]			= _yAxis->getAxisData();
	newImgOptions["editOptions"]	= newEditOptions;

	_analysis->editImage(newImgOptions);
}

void PlotEditorModel::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}

void PlotEditorModel::setName(QString name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged(_name);
	somethingChanged();
}

QString	PlotEditorModel::data() const
{
	static QString prefix = "file://" + tq(TempFiles::sessionDirName()) + "/";
	return prefix + _data;
}

void PlotEditorModel::setData(QString data)
{
	if (_data == data)
		return;

	_data = data;
	emit dataChanged(_data);
	somethingChanged();
}

void PlotEditorModel::setTitle(QString title)
{
	if (_title == title)
		return;

	_title = title;
	emit titleChanged(_title);
	somethingChanged();
}

void PlotEditorModel::setWidth(int width)
{
	if (_width == width)
		return;

	_width = width;
	emit widthChanged(_width);
}

void PlotEditorModel::setHeight(int height)
{
	if (_height == height)
		return;

	_height = height;
	emit heightChanged(_height);
}
