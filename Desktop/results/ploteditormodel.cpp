#include "ploteditormodel.h"
#include "analysis/analyses.h"
#include "utilities/qutils.h"
#include "gui/preferencesmodel.h"
#include "log.h"
#include "tempfiles.h"
#include <QDir>
#include "gui/messageforwarder.h"

namespace PlotEditor
{

int PlotEditorModel::_editRequest = 0;

PlotEditorModel::PlotEditorModel()
	: QObject(Analyses::analyses())
{
	_xAxis = new AxisModel(this, true);
	_yAxis = new AxisModel(this, true);
	_currentAxis = _xAxis;
	_ppi   = PreferencesModel::prefs()->plotPPI();

	connect(_xAxis,		&AxisModel::somethingChanged,	this,	&PlotEditorModel::somethingChanged);
	connect(_yAxis,		&AxisModel::somethingChanged,	this,	&PlotEditorModel::somethingChanged);
	connect(_xAxis,		&AxisModel::addToUndoStack,		this,	&PlotEditorModel::addToUndoStack);
	connect(_yAxis,		&AxisModel::addToUndoStack,		this,	&PlotEditorModel::addToUndoStack);

}

void PlotEditorModel::showPlotEditor(int id, QString options)
{
	_analysis		= Analyses::analyses()->get(id);
	_imgOptions		= Json::objectValue;
	Json::Reader().parse(fq(options), _imgOptions);

	setLoading(true);
	_prevImgOptions	= Json::nullValue;
	
	//maybe the following checks are a bit extreme but whatever
	if(!_analysis || !_imgOptions.isMember("type") || _imgOptions["type"].type() != Json::stringValue || _imgOptions["type"] != "interactive")
		return;

	processImgOptions();
	_originalImgOps = generateImgOptions();

	if (_validOptions)
		setVisible(true);
	setLoading(false);
}

void PlotEditorModel::updatePlotEditor(Analysis *analysis)
{
	if (_analysis != analysis)	return;

	setLoading(true);
	_editOptions = analysis->editOptionsOfPlot(_name.toStdString());
	_xAxis->setAxisData(_editOptions["xAxis"]);
	_yAxis->setAxisData(_editOptions["yAxis"]);
	setLoading(false);
}

void PlotEditorModel::cancelPlot()
{
	// After this call, the plotEditor is closed. So no need to set all members here.
	_imgOptions		= _prevImgOptions = _originalImgOps;
	_editOptions	= _originalImgOps["editOptions"];
	_editOptions["resetPlot"] = false;
	_analysis->editImage(_originalImgOps);
}

void PlotEditorModel::resetDefaults()
{
	Json::Value resetDefaultValue = _originalImgOps;
	resetDefaultValue["editOptions"]["resetPlot"] = true;
	_analysis->editImage(resetDefaultValue);
}

void PlotEditorModel::savePlot() const
{
	emit saveImage(int(_analysis->id()), tq(generateImgOptions().toStyledString()));
}

void PlotEditorModel::reset()
{
	_analysis		=	nullptr;
	_imgOptions		=	Json::nullValue;
	_editOptions	=	Json::nullValue;
	setName(			"");
	setData(			"");
	setTitle(			"");
	setWidth(			100);
	setHeight(			100);

	setAxisType(AxisType::Xaxis);
	_undo = std::stack<undoRedoData>();
	_redo = std::stack<undoRedoData>();
	emit unOrRedoEnabledChanged();
}

void PlotEditorModel::processImgOptions()
{
	setName(	tq(			_imgOptions.get(	"name",			"").asString()));
	setData(	tq(			_imgOptions.get(	"data",			"").asString()));
	setTitle(	tq(			_imgOptions.get(	"title",		"").asString()));
	setWidth(				_imgOptions.get(	"width",		100).asInt());
	setHeight(				_imgOptions.get(	"height",		100).asInt());

	//_editOptions		=	_imgOptions.get(	"editOptions",	Json::objectValue);
	_editOptions		=	_name == "" || !_analysis ? Json::objectValue : _analysis->editOptionsOfPlot(_name.toStdString());
	_editOptions["resetPlot"] = false;

	std::string reasonOptionsAreInvalid = _editOptions.get("reasonNotEditable", "").asString();

	_validOptions = reasonOptionsAreInvalid.empty();
	if (!_validOptions)
	{
		MessageForwarder::showWarning(reasonOptionsAreInvalid);
		return;
	}

	Json::Value	xAxis	=	_editOptions.get(	"xAxis",		Json::objectValue),
				yAxis	=	_editOptions.get(	"yAxis",		Json::objectValue);

	_xAxis->setAxisData(xAxis);
	_yAxis->setAxisData(yAxis);

	_coordinates.loadCoordinates(_editOptions.get("coordinates", Json::objectValue)); // To Do Vincent Pedata: is this the right json object?

}

Json::Value PlotEditorModel::generateImgOptions() const
{
	Json::Value		imgOptions	= _imgOptions;

	imgOptions["editOptions"]	= generateEditOptions();

	imgOptions["name"]			= name().toStdString();
	imgOptions["data"]			= data().toStdString();
	imgOptions["title"]			= title().toStdString();
	imgOptions["width"]			= width();
	imgOptions["height"]		= height();
	imgOptions["request"]		= _editRequest++;

	return imgOptions;
}

Json::Value PlotEditorModel::generateEditOptions() const
{
	Json::Value		editOptions = _editOptions;

	editOptions["xAxis"]		= _xAxis->getAxisData();
	editOptions["yAxis"]		= _yAxis->getAxisData();

	// To Do Vincent Pedata: Do we need to send the coordinates back? No right?

	return editOptions;
}

void PlotEditorModel::somethingChanged()
{
	if(_loading || !_visible) return;

	Json::Value newImgOptions = generateImgOptions();

	if(newImgOptions != _prevImgOptions)
	{
		_prevImgOptions = newImgOptions;
		_analysis->editImage(_prevImgOptions);
	}
}


void PlotEditorModel::addToUndoStack()
{
	if(_loading || !_visible) return;

	Json::Value options = generateImgOptions();

	if (_undo.empty() || _undo.top().options != options)
		_undo.push(undoRedoData{_axisType, _advanced, options});

	_redo = std::stack<undoRedoData>();
	
	emit unOrRedoEnabledChanged();
}

void PlotEditorModel::undoSomething()
{
	if (!_undo.empty())
	{

		_redo.push(undoRedoData{_undo.top().currentAxis, _undo.top().advanced, generateImgOptions()});

		undoRedoData newData = _undo.top();
		_undo.pop();
		applyChangesFromUndoOrRedo(newData);


	}
}

void PlotEditorModel::redoSomething()
{
	if (!_redo.empty())
	{

		_undo.push(undoRedoData{_axisType, _advanced, generateImgOptions()});

		undoRedoData newData = _redo.top();
		_redo.pop();
		applyChangesFromUndoOrRedo(newData);


	}
}

void PlotEditorModel::applyChangesFromUndoOrRedo(const undoRedoData& newData)
{
	_imgOptions = newData.options;
	setAxisType(newData.currentAxis);
	setAdvanced(newData.advanced);

	setLoading(true);
	_editOptions = _imgOptions["editOptions"];

	_xAxis->setAxisData(_editOptions["xAxis"]);
	_yAxis->setAxisData(_editOptions["yAxis"]);

	_prevImgOptions = _imgOptions;
	_analysis->editImage(_prevImgOptions);
	setLoading(false);

	emit unOrRedoEnabledChanged();
}

void PlotEditorModel::setAxisType(const AxisType axisType)
{

	if (_axisType == axisType)
		return;

	_axisType = axisType;
	switch (_axisType)
	{
		case AxisType::Xaxis:	_currentAxis = _xAxis;		break;
		case AxisType::Yaxis:	_currentAxis = _yAxis;		break;
	}

	emit currentAxisChanged(_currentAxis);
	emit axisTypeChanged(_axisType);
}

void PlotEditorModel::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);

	if(!_visible)
		reset();
}

void PlotEditorModel::setName(const QString & name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged(_name);
	somethingChanged();
}

void PlotEditorModel::refresh()
{
	if(!_analysis)
		return;

	//Lets make sure the plot gets reloaded by QML
	_goBlank = true;
	emit dataChanged();
	_goBlank = false;
	emit dataChanged();

	_analysis->setEditOptionsOfPlot(_name.toStdString(), generateEditOptions());
}

QUrl PlotEditorModel::imgFile() const
{
	if(!_analysis || _goBlank)
		return QUrl("");

	QString pad(tq(TempFiles::sessionDirName()) + "/" + _data);
		
	return QUrl::fromLocalFile(pad);
}

void PlotEditorModel::setData(const QString & data)
{
	if (_data == data)
		return;

	_data = data;
	emit dataChanged();
	somethingChanged();
}

void PlotEditorModel::setTitle(const QString & title)
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

QString PlotEditorModel::clickHitsElement(double x, double y) const
{
	std::string elementName;
	return 	_coordinates.elementHit(x, y, elementName) ? tq(elementName) : "";
}

void PlotEditorModel::setLoading(bool loading)
{
	if (_loading == loading)
		return;
	
	_loading = loading;
	emit loadingChanged(_loading);
}

void PlotEditorModel::setAdvanced(bool advanced)
{
	if (_advanced == advanced)
		return;

	// ease the transition to advanced mode by simplifying where possible
	if (advanced)
	{
		_xAxis->simplifyLimitsType();
		_yAxis->simplifyLimitsType();
	}

	_advanced = advanced;
	emit advancedChanged(_advanced);
}

}
