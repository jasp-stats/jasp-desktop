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
	_ppi   = PreferencesModel::prefs()->plotPPI();
	// _resetPlot is always false, it should only be set to TRUE from QML

	connect(_xAxis, &AxisModel::somethingChanged, this, &PlotEditorModel::somethingChanged);
	connect(_yAxis, &AxisModel::somethingChanged, this, &PlotEditorModel::somethingChanged);
}

void PlotEditorModel::showPlotEditor(int id, QString options)
{
	setLoading(true);
	_analysisId		= id;
	_analysis		= Analyses::analyses()->get(id);
	_imgOptions		= Json::objectValue;
	_prevImgOptions	= Json::nullValue;
	_options		= options;

	Json::Reader().parse(fq(options), _imgOptions);
	
	//maybe the following checks are a bit extreme but whatever
	if(!_analysis || !_imgOptions.isMember("type") || _imgOptions["type"].type() != Json::stringValue || _imgOptions["type"] != "interactive")
		return;

	_originalImgOps = _imgOptions;
	_lastAnalID		= _analysisId;
	
	processImgOptions();

	// don't signal this to qml
	Log::log() << "initial edit options for undo are: " + generateImgOptions().toStyledString() << std::endl;
	_undo.push(generateImgOptions());

	setVisible(true);
	setLoading(false);
}

void PlotEditorModel::resetPlot()
{
	setLoading(true);
	reset();
	showPlotEditor(_lastAnalID, tq(_originalImgOps.toStyledString()));	
	emit somethingChanged();
}

void PlotEditorModel::savePlot() const
{
	emit saveImage(_analysisId, _options);
}

void PlotEditorModel::reset()
{
	_analysisId		=	-1;
	_analysis		=	nullptr;
	_imgOptions		=	Json::nullValue;
	_editOptions	=	Json::nullValue;
	setName(			"");
	setData(			"");
	setTitle(			"");
	setWidth(			100);
	setHeight(			100);

	clear(_undo);
	clear(_redo);
	setUndoEnabled(false);
	setRedoEnabled(false);
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

void PlotEditorModel::clear(std::stack<Json::Value>& x)
{
	while(!x.empty())
		x.pop();
}

void PlotEditorModel::somethingChanged()
{
	if(_loading || !_visible) return;

	Json::Value newImgOptions = generateImgOptions();

	if(newImgOptions != _prevImgOptions)
	{

		_prevImgOptions = newImgOptions;
		// ensure that the undo stack is unique and the first previous image options are skipped (since those are null)
		if (_undo.empty() || _undo.top() != _prevImgOptions)
		{
			Log::log() << "_undo.push(_prevImgOptions): " << _prevImgOptions.toStyledString() << std::endl;
			_undo.push(_prevImgOptions);
			setUndoEnabled(true);
		}

		clear(_redo);
		setRedoEnabled(false);


		_analysis->editImage(_prevImgOptions);
	}
}



void PlotEditorModel::undoSomething()
{
	if (_undo.size() > 1)
	{
		_redo.push(_undo.top());
		_undo.pop();
		_imgOptions = _undo.top();

		setRedoEnabled(true);
		setUndoEnabled(_undo.size() > 1);

		applyChangesFromUndoOrRedo();
	}
}

void PlotEditorModel::redoSomething()
{
	if (!_redo.empty())
	{
		_imgOptions = _redo.top();
		_redo.pop();
		setRedoEnabled(!_redo.empty());

		_undo.push(_imgOptions);
		setUndoEnabled(true);

		applyChangesFromUndoOrRedo();
	}
}

void PlotEditorModel::applyChangesFromUndoOrRedo()
{
	_editOptions = _imgOptions["editOptions"];

	_xAxis->setAxisData(_editOptions["xAxis"]);
	_yAxis->setAxisData(_editOptions["yAxis"]);

	_prevImgOptions = _imgOptions;
	_analysis->editImage(_prevImgOptions);
}

void PlotEditorModel::setUndoEnabled(bool undoEnabled)
{
	if (_undoEnabled == undoEnabled)
		return;

	_undoEnabled = undoEnabled;
	emit undoEnabledChanged(_undoEnabled);
}

void PlotEditorModel::setRedoEnabled(bool redoEnabled)
{
	if (_redoEnabled == redoEnabled)
		return;

	_redoEnabled = redoEnabled;
	emit redoEnabledChanged(_redoEnabled);
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

	_advanced = advanced;
	emit advancedChanged(_advanced);
}

}
