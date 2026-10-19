#include "ploteditormodel.h"
#include "analysis/analyses.h"
#include "qutils.h"
#include "gui/preferencesmodel.h"
#include "log.h"
#include "tempfiles.h"
#include <QDir>
#include "utilities/messageforwarder.h"

namespace PlotEditor
{

int PlotEditorModel::_editRequest = 0;

PlotEditorModel::PlotEditorModel()
	: QObject(Analyses::analyses())
{
	_xAxis			= new AxisModel(this, true);
	_yAxis			= new AxisModel(this, true);
	_currentAxis	= _xAxis;
	_ppi			= PreferencesModel::prefs()->plotPPI();
	_references		= new References(this);

	connect(_xAxis,			&AxisModel::somethingChanged,	this,	&PlotEditorModel::somethingChanged);
	connect(_yAxis,			&AxisModel::somethingChanged,	this,	&PlotEditorModel::somethingChanged);
	connect(_references,	&References::somethingChanged,	this,	&PlotEditorModel::somethingChanged);
	connect(_references,	&References::addToUndoStack,	this,	&PlotEditorModel::addToUndoStack);
	connect(_xAxis,			&AxisModel::addToUndoStack,		this,	&PlotEditorModel::addToUndoStack);
	connect(_yAxis,			&AxisModel::addToUndoStack,		this,	&PlotEditorModel::addToUndoStack);

	_debounceTimer.setSingleShot(true);
	connect(&_debounceTimer, &QTimer::timeout, this, &PlotEditorModel::applyPendingChanges);

}

void PlotEditorModel::showPlotEditor(int id, QString options)
{
	_analysis		= Analyses::analyses()->get(size_t(id));
	_imgOptions		= Json::objectValue;
	Json::Reader().parse(fq(options), _imgOptions);

	setLoading(true);
	
	//maybe the following checks are a bit extreme but whatever
	if(!_analysis || !_imgOptions.isMember("type") || _imgOptions["type"].type() != Json::stringValue || _imgOptions["type"] != "interactive")
		return;
	
	if (_analysis->needsRefresh())
	{
		if (	_analysis->storedWithoutState() 
			?	MessageForwarder::showYesNo(tr("Stored without state"),		tr("This analysis was stored without state, to edit the plot it must be refreshed first.\n\nRefresh the analysis?"))
			:	MessageForwarder::showYesNo(tr("Version incompatibility"),	tr("This analysis was created in an older version of JASP, to edit the plot it must be refreshed first.\n\nRefresh the analysis?"))
			)
			_analysis->refresh();
	}
	else
	{

		setup();
	
		if (_validOptions)
			setVisible(true);
	}
	setLoading(false);
}

void PlotEditorModel::setup()
{
	setName(	tq(			_imgOptions.get(	"name",			"").asString()));
	setData(	tq(			_imgOptions.get(	"data",			"").asString()));
	setTitle(	tq(			_imgOptions.get(	"title",		"").asString()));
	setWidth(				_imgOptions.get(	"width",		100).asInt());
	setHeight(				_imgOptions.get(	"height",		100).asInt());

	Json::Value editOptions		=	_name == "" || !_analysis ? Json::objectValue : _analysis->editOptionsOfPlotFromEdits(_name.toStdString());
	editOptions["resetPlot"] = false;
	_imgOptions["editOptions"] = editOptions;

	std::string reasonOptionsAreInvalid = editOptions.get("reasonNotEditable", "").asString();

	_validOptions = reasonOptionsAreInvalid.empty();
	if (!_validOptions)
	{
		MessageForwarder::showWarning(reasonOptionsAreInvalid);
		return;
	}

	_xAxis		->setAxisData(	editOptions.get("xAxis",		Json::objectValue));
	_yAxis		->setAxisData(	editOptions.get("yAxis",		Json::objectValue));
	_references	->fromJson(		editOptions.get("references",	Json::arrayValue));

	_originalImgOps = _imgOptions = generateImgOptions();
}

void PlotEditorModel::cancelPlot()
{
	if (_imgOptions != _originalImgOps)
		updatePlot(_originalImgOps);
}

void PlotEditorModel::resetDefaults()
{
	Json::Value resetDefaultValue = _originalImgOps;
	resetDefaultValue["editOptions"]["resetPlot"] = true;
	updatePlot(resetDefaultValue);
}

void PlotEditorModel::savePlot() const
{
	emit saveImage(int(_analysis->id()), tq(_imgOptions.toStyledString()));
}

void PlotEditorModel::reset()
{
	_analysis		=	nullptr;
	_imgOptions		=	Json::nullValue;
	setName(			"");
	setData(			"");
	setTitle(			"");
	setWidth(			100);
	setHeight(			100);

	setAxisType(AxisType::Xaxis);
	_undo = std::stack<undoRedoData>();
	_redo = std::stack<undoRedoData>();
	emit unOrRedoEnabledChanged();

	_debounceTimer.stop();
	_debouncePending = false;
	setUpdating(false);
}


Json::Value PlotEditorModel::generateImgOptions() const
{
	Json::Value newOptions = _imgOptions;
	newOptions["editOptions"]["xAxis"]		= _xAxis->getAxisData();
	newOptions["editOptions"]["yAxis"]		= _yAxis->getAxisData();
	newOptions["editOptions"]["references"]	= _references->toJson();

	newOptions["name"]		= name().toStdString();
	newOptions["data"]		= data().toStdString();
	newOptions["title"]		= title().toStdString();
	newOptions["width"]		= width();
	newOptions["height"]	= height();

	return newOptions;
}

void PlotEditorModel::updatePlot(Json::Value& imageOptions)
{
	imageOptions["request"] = _editRequest;
	_editedImgsMap[_editRequest] = imageOptions["editOptions"];
	_editRequest++;
	_analysis->editImage(imageOptions);

	setUpdating(true);
}

void PlotEditorModel::updateOptions(Analysis *analysis)
{
	if (_analysis != analysis)	return;

	int request = _analysis->imgResults()["request"].asInt();
	const Json::Value& optionsSend = _editedImgsMap[request];
	Json::Value optionsReceived = analysis->editOptionsOfPlotFromEdits(_name.toStdString());
	// After editing a plot the engine returns the current edit options.
	// These options may differ from the ones send to the engine (currently only when resetDefault is called, but later there could have other situation):
	// in this case the plot editor options must be updated.
	// It is important not to update the plot editor unnecessarily: as it can take a little while to update a plot,
	// the user during this time may have changed other options, and the update of the plot editor would undo the last update of the user
	// (temporarily, until this last change is returned from the engine), making the user confused.
	// Unfortunately most of the time the optionsSend and optionsReceived json differs because of real to int change (100.0 becomes 100 e.g)
	// So to check (optionsSend == optionsReceived) will not work.
	// Until a better solution, the plot editor will only be updated after a resetPlot

	Log::log() << "OPTIONS SEND" << optionsSend.toStyledString() << std::endl;
	Log::log() << "OPTIONS RECEIVED" << optionsReceived.toStyledString() << std::endl;
	if (optionsSend["resetPlot"] == true)
	{
		setBlockChanges(true);
		_imgOptions["editOptions"] = optionsReceived;
		_xAxis->setAxisData(_imgOptions["editOptions"]["xAxis"]);
		_yAxis->setAxisData(_imgOptions["editOptions"]["yAxis"]);
		setBlockChanges(false);
	}

	_editedImgsMap.erase(request);

	// If a debounced update is already queued, sync the request ID in _imgOptions
	// to the just-processed response so that Analysis::imageEdited() does not
	// trigger a redundant re-send. The debounce timer will handle sending the
	// latest changes when it fires.
	if (_debouncePending)
	{
		if (_analysis->imgResults().isMember("request"))
			_imgOptions["request"] = _analysis->imgResults()["request"];
		// Keep _updating = true since the debounce will send another request shortly
	}
	else
		setUpdating(false);
}

void PlotEditorModel::somethingChanged()
{
	if(_loading || !_visible || _blockChanges) return;

	Json::Value newImgOptions = generateImgOptions();

	if(newImgOptions != _imgOptions)
	{
		_imgOptions = newImgOptions;

		// Debounce: wait for a pause in changes before sending to the engine.
		// This prevents flooding the engine during rapid slider dragging, etc.
		_debouncePending = true;
		_debounceTimer.start(250);
	}
}

void PlotEditorModel::applyPendingChanges()
{
	_debouncePending = false;
	_imgOptions["intermediate"] = true;
	updatePlot(_imgOptions);
}


void PlotEditorModel::addToUndoStack()
{
	if(_loading || !_visible) return;

	Json::Value options = generateImgOptions();

	if (_undo.empty() || _undo.top().options != options)
		_undo.push(undoRedoData{_axisType, options});

	_redo = std::stack<undoRedoData>();
	
	emit unOrRedoEnabledChanged();
}

void PlotEditorModel::undoSomething()
{
	if (!_undo.empty())
	{

		_redo.push(undoRedoData{_undo.top().currentAxis, generateImgOptions()});

		undoRedoData newData = _undo.top();
		_undo.pop();
		applyChangesFromUndoOrRedo(newData);


	}
}

void PlotEditorModel::redoSomething()
{
	if (!_redo.empty())
	{
		_undo.push(undoRedoData{_axisType, generateImgOptions()});

		undoRedoData newData = _redo.top();
		_redo.pop();
		applyChangesFromUndoOrRedo(newData);
	}
}

void PlotEditorModel::applyChangesFromUndoOrRedo(const undoRedoData& newData)
{
	_imgOptions = newData.options;
	setAxisType(newData.currentAxis);

	setBlockChanges(true);

	_xAxis->setAxisData(_imgOptions["editOptions"]["xAxis"]);
	_yAxis->setAxisData(_imgOptions["editOptions"]["yAxis"]);

	updatePlot(_imgOptions);
	setBlockChanges(false);

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

	_analysis->setEditOptionsOfPlot(_name.toStdString(), _imgOptions["editOptions"]);

	//Lets make sure the plot gets reloaded by QML
	_goBlank = true;
	emit dataChanged();
	_goBlank = false;
	emit dataChanged();

	_xAxis->refresh();
	_yAxis->refresh();
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

/*
QString PlotEditorModel::clickHitsElement(double x, double y) const
{
	std::string elementName;
	return 	_coordinates.elementHit(x, y, elementName) ? tq(elementName) : "";
}*/

void PlotEditorModel::setLoading(bool loading)
{
	if (_loading == loading)
		return;
	
	_loading = loading;
	emit loadingChanged(_loading);
}

void PlotEditorModel::setUpdating(bool updating)
{
	if (_updating == updating)
		return;

	_updating = updating;
	emit updatingChanged(_updating);
}

References *PlotEditorModel::references() const
{
	return _references;
}

}
