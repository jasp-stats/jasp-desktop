//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "analysisform.h"
#include "knownissues.h"
#include "boundcontrol.h"
#include "utilities/qutils.h"
#include "widgets/listmodeltermsavailable.h"
#include "widgets/jasplistcontrol.h"
#include "widgets/expanderbuttonbase.h"
#include "log.h"
#include "jaspcontrol.h"
#include "data/columnsmodel.h"

#include <QQmlProperty>
#include <QQmlContext>
#include <QTimer>

using namespace std;

AnalysisForm::AnalysisForm(QQuickItem *parent) : QQuickItem(parent)
{
	setObjectName("AnalysisForm");
	connect(this,					&AnalysisForm::infoChanged,			this, &AnalysisForm::helpMDChanged			);
	connect(this,					&AnalysisForm::formCompleted,		this, &AnalysisForm::formCompletedHandler   );
	connect(this,					&AnalysisForm::analysisChanged,		this, &AnalysisForm::knownIssuesUpdated,	Qt::QueuedConnection);
	connect(KnownIssues::issues(),	&KnownIssues::knownIssuesUpdated,	this, &AnalysisForm::knownIssuesUpdated,	Qt::QueuedConnection);
}

QVariant AnalysisForm::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	ColumnsModel* colModel = ColumnsModel::singleton();
	if (!colModel) return QVariant();

	try {
		int i = colModel->getColumnIndex(term.asString());
		if (i < 0)									return "";

		QModelIndex index = colModel->index(i, 0);
		switch(info)
		{
		case VariableInfo::VariableType:			return colModel->data(index, ColumnsModel::ColumnTypeRole);
		case VariableInfo::VariableTypeName:		return columnTypeToQString(columnType((colModel->data(index, ColumnsModel::ColumnTypeRole)).toInt()));
		case VariableInfo::VariableTypeIcon:		return colModel->data(index, ColumnsModel::IconSourceRole);
		case VariableInfo::VariableTypeDisabledIcon: return colModel->data(index, ColumnsModel::DisabledIconSourceRole);
		case VariableInfo::VariableTypeInactiveIcon: return colModel->data(index, ColumnsModel::InactiveIconSourceRole);
		case VariableInfo::Labels:					return	colModel->data(index, ColumnsModel::LabelsRole);
		}
	}
	catch(columnNotFound & e) {} //just return an empty QVariant right?
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}
	return QVariant("");

}

void AnalysisForm::runRScript(QString script, QString controlName, bool whiteListedVersion)
{
	if(_analysis && !_removed && _signalValueChangedBlocked == 0)
		emit _analysis->sendRScript(_analysis, script, controlName, whiteListedVersion);
}

void AnalysisForm::refreshAnalysis()
{
	_analysis->refresh();
}

void AnalysisForm::runAnalysis()
{
	_analysis->run();
	refreshTableViewModels();
}

void AnalysisForm::itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value)
{
	if (change == ItemChange::ItemSceneChange && !value.window)
		cleanUpForm();
	QQuickItem::itemChange(change, value);
}

void AnalysisForm::cleanUpForm()
{
	if (!_removed)
	{
		_removed = true;
		for (JASPControl* control : _dependsOrderedCtrls)
			// controls will be automatically deleted by the deletion of AnalysisForm
			// But they must be first disconnected: sometimes an event seems to be triggered before the item is completely destroyed
			control->cleanUp();
	}
}

void AnalysisForm::runScriptRequestDone(const QString& result, const QString& controlName)
{	
	if(_removed)
		return;

	JASPControl* item = getControl(controlName);
	if (!item)
	{
		QStringList composedName = controlName.split(".");
		if (composedName.length() == 3)
		{
			JASPControl* parentControl = getControl(composedName[0]);
			if (parentControl)
				item = dynamic_cast<JASPControl*>(parentControl->getChildControl(composedName[1], composedName[2]));
		}
	}

	if (item)
		item->rScriptDoneHandler(result);
	else
		Log::log() << "Unknown item " << controlName.toStdString() << std::endl;
}

void AnalysisForm::addControl(JASPControl *control)
{
	const QString & name = control->name();

	if (name.isEmpty() && control->isBound())
	{
		QString label = control->humanFriendlyLabel();

		if (!label.isEmpty())	addFormError(tr("Control with label '%1' has no name").arg(label));
		else					addFormError(tr("A control has no name"));

		control->setHasError(true);
	}

	if (!name.isEmpty() && control->nameMustBeUnique())
	{
		if (_controls.keys().contains(name))
		{
			addFormError(tr("2 controls have the same name: %1").arg(name));
			control			->	setHasWarning(true);
			_controls[name]	->	setHasWarning(true);
		}
		else
			_controls[name] = control;
	}

	if (_analysis)
	{
		connect(control, &JASPControl::requestColumnCreation, _analysis, &Analysis::requestColumnCreationHandler);
	}

	if (control->controlType() == JASPControl::ControlType::Expander)
	{
		ExpanderButtonBase* expander = dynamic_cast<ExpanderButtonBase*>(control);
		_expanders.push_back(expander);
	}
}

void AnalysisForm::addColumnControl(JASPControl* control, bool isComputed)
{
	if (isComputed)
	{
		connect(control, &JASPControl::requestComputedColumnCreation, _analysis, &Analysis::requestComputedColumnCreationHandler);
		connect(control, &JASPControl::requestComputedColumnDestruction, _analysis, &Analysis::requestComputedColumnDestructionHandler);
	}
	else
		connect(control, &JASPControl::requestColumnCreation, _analysis, &Analysis::requestColumnCreationHandler);
}

void AnalysisForm::_setUpControls()
{
	_orderExpanders();
	_setUpModels();
	_setUp();
}

void AnalysisForm::_setUpModels()
{
	for (JASPControl* control : _controls.values())
	{
		JASPListControl*	listControl = qobject_cast<JASPListControl*>(control);
		if (listControl)	listControl->setUpModel();
	}
}

void AnalysisForm::sortControls(QList<JASPControl*>& controls)
{
	for (JASPControl* control : controls)
	{
		std::vector<JASPControl*> depends(control->depends().begin(), control->depends().end());

		// By adding at the end of the vector new dependencies, this makes sure that these dependencies of these new dependencies are
		// added and so on recursively, so that the 'depends' set of each control gets all (direct or indirect) controls it depends on.
		// (that's why we cannot use a for-each loop here or an iterator, because it loops on a vector that is growing during the loop).
		// Afterwards, if a control depends (directly or indirectly) of another control, the number of elements in its 'depends' set is then
		// automatically strictly bigger than the 'depends' set of all controls it depends on.
		// We have then simply to use the size of their 'depends' set, to sort the controls.
		for (size_t index = 0; index < depends.size(); index++)
		{
			JASPControl					* depend		= depends[index];
			const std::set<JASPControl*>	& dependdepends = depend->depends();

			for (JASPControl* dependdepend : dependdepends)
				if (dependdepend == control)
					addFormError(tq("Circular dependency between control %1 and %2").arg(control->name()).arg(depend->name()));
				else if (control->addDependency(dependdepend))
					depends.push_back(dependdepend);
		}
	}

	std::sort(controls.begin(), controls.end(),
		[](JASPControl* a, JASPControl* b) {
			return a->depends().size() < b->depends().size();
		});
}

void AnalysisForm::_setUp()
{
	QList<JASPControl*> controls = _controls.values();

	// set the order of the BoundItems according to their dependencies (for binding purpose)
	for (JASPControl* control : controls)
		control->setUp();

	sortControls(controls);

	for (JASPControl* control : controls)
	{
		_dependsOrderedCtrls.push_back(control);
		connect(control, &JASPControl::helpMDChanged, this, &AnalysisForm::helpMDChanged);
	}

	emit helpMDChanged(); //Because we just got info on our lovely children in _orderedControls
}

void AnalysisForm::_orderExpanders()
{
	for (ExpanderButtonBase* expander : _expanders)
	{
		bool foundExpander = false;
		for (QObject* sibling : expander->parent()->parent()->children())
		{
			if (sibling->objectName() == "Section")
			{
				QObject			* button	= sibling->property("button").value<QObject*>();
				JASPControl	* control	= qobject_cast<JASPControl*>(button);
				if (control && control->controlType() == JASPControl::ControlType::Expander)
				{
					if (foundExpander)
					{
						_nextExpanderMap[expander] = dynamic_cast<ExpanderButtonBase*>(control);
						break;
					}
					if (control == expander)
						foundExpander = true;
				}
			}
		}
		expander->setUp();
	}
}

void AnalysisForm::reset()
{
	_analysis->clearOptions();
    _analysis->reload();
}

void AnalysisForm::exportResults()
{
    _analysis->exportResults();
}

QString AnalysisForm::msgsListToString(const QStringList & list) const
{
	if(list.length() == 0) return "";

	QString text = "<ul style=\"margin-bottom:0px\">";

	for (const QString& errorMessage : list)
		text.append("<li>").append(errorMessage).append("</li>");

	text.append("</ul>");

	return text;
}

void AnalysisForm::setInfo(QString info)
{
	if (_info == info)
		return;

	_info = info;
	emit infoChanged();
}

QString AnalysisForm::_getControlLabel(QString controlName)
{
	return _controls[controlName]->humanFriendlyLabel();
}

void AnalysisForm::_addLoadingError(QStringList wrongJson)
{
	if (wrongJson.size() > 0)
	{
		QString errorMsg;
		if (wrongJson.size() == 1)
		{
			errorMsg = tr("Component %1 was loaded with the wrong type of value and has been reset to its default value.").arg(_getControlLabel(wrongJson[0]));
			errorMsg += "<br>";
		}
		else if (wrongJson.size() < 4)
		{
			QString names = "<ul>";
			for(const QString & controlName : wrongJson)
				names += "<li>" + _getControlLabel(controlName) + "</li>";
			names += "</ul>";

			errorMsg = tr("These components were loaded with the wrong type of value and have been reset to their default values:%1").arg(names);
		}
		else
		{
			errorMsg = tr("Many components were loaded with the wrong type of value and have been reset to their default values.");
			errorMsg += "<br>";
		}

		errorMsg += tr("The file probably comes from an older version of JASP.");
		errorMsg += "<br>" + tr("That means that the results currently displayed do not correspond to the options selected.");
		errorMsg += "<br>" + tr("Refreshing the analysis may change the results.");
		addFormError(errorMsg);
	}
}

void AnalysisForm::bindTo()
{
	unbind();

	const Json::Value & defaultOptions = _analysis->isDuplicate() ? _analysis->boundValues() : _analysis->optionsFromJASPFile();
	QVector<ListModelAvailableInterface*> availableModelsToBeReset;

	std::set<std::string> controlsJsonWrong;
	
	for (JASPControl* control : _dependsOrderedCtrls)
	{
		BoundControl* boundControl = control->boundControl();
		JASPListControl* listControl = dynamic_cast<JASPListControl *>(control);

		if (boundControl)
		{
			std::string name = control->name().toStdString();
			Json::Value optionValue =  defaultOptions != Json::nullValue ? defaultOptions[name] : Json::nullValue;

			if (optionValue != Json::nullValue && !boundControl->isJsonValid(optionValue))
			{
				optionValue = Json::nullValue;
				control->setHasWarning(true);
				controlsJsonWrong.insert(name);
			}

			if (optionValue == Json::nullValue)
				optionValue = boundControl->createJson();

			boundControl->bindTo(optionValue);
		}

		if (listControl && listControl->hasSource())
		{
			ListModelAvailableInterface* availableModel = qobject_cast<ListModelAvailableInterface*>(listControl->model());
			// The availableList control are not bound with options, but they have to be updated from their sources when the form is initialized.
			// The availableList cannot signal their assigned models now because they are not yet bound (the controls are ordered by dependency)
			// When the options come from a JASP file, an assigned model needs sometimes the available model (eg. to determine the kind of terms they have).
			// So in this case resetTermsFromSourceModels has to be called now but with updateAssigned argument set to false.
			// When the options come from default options (from source models), the availableList needs sometimes to signal their assigned models (e.g. when addAvailableVariablesToAssigned if true).
			// As their assigned models are not yet bound, resetTermsFromSourceModels (with updateAssigned argument set to true) must be called afterwards.
			if (availableModel)
			{
				if (defaultOptions != Json::nullValue || _analysis->isDuplicate())
					availableModel->resetTermsFromSources(false);
				else
					availableModelsToBeReset.push_back(availableModel);
			}
		}

		control->setInitialized();
	}

	for (ListModelAvailableInterface* availableModel : availableModelsToBeReset)
		availableModel->resetTermsFromSources(true);

	_addLoadingError(tql(controlsJsonWrong));

	//Ok we can only set the warnings on the components now, because otherwise _addLoadingError() will add a big fat red warning on top of the analysisform without reason...
	for (JASPControl* control : _dependsOrderedCtrls)
	{
		QString upgradeMsg(tq(_analysis->upgradeMsgsForOption(fq(control->name()))));

		if(upgradeMsg != "")
			control->addControlWarning(upgradeMsg);
	}

	//Also check for a warning to show above the analysis:
	QString upgradeMsg(tq(_analysis->upgradeMsgsForOption("")));

	if(upgradeMsg != "")
		addFormError(upgradeMsg);

	_analysis->setOptionsBound(true);
}

void AnalysisForm::unbind()
{
	_analysis->setOptionsBound(false);
}

void AnalysisForm::addFormError(const QString &error)
{
	_formErrors.append(error);
	emit errorsChanged();
}

QQuickItem* AnalysisForm::_getControlErrorMessageOfControl(JASPControl* jaspControl)
{
	QQuickItem* result = nullptr;

	for (QQuickItem* item : _controlErrorMessageCache)
		if (item->parentItem() == jaspControl)
		{
			result = item;
			break;
		}

	return result;
}

//This should be moved to JASPControl maybe?
//Maybe even to full QML? Why don't we just use a loader...
void AnalysisForm::addControlError(JASPControl* control, QString message, bool temporary, bool warning)
{
	if (!control) return;

	if (!message.isEmpty())
	{
		QQuickItem*	controlErrorMessageItem = nullptr;

		for (QQuickItem* item : _controlErrorMessageCache)
		{
			JASPControl* errorControl = item->property("control").value<JASPControl*>();
			if (errorControl == control || !errorControl)
			{
				controlErrorMessageItem = item;
				break;
			}
		}

		if (!controlErrorMessageItem)
		{
			// Cannot instantiate _controlErrorMessageComponent in the constructor (it crashes), and it might be too late in the formCompletedHandler since error can be generated earlier
			// So create it when it is needed for the first time.
			if (!_controlErrorMessageComponent)
				_controlErrorMessageComponent = new QQmlComponent(qmlEngine(this), "qrc:///components/JASP/Widgets/ControlErrorMessage.qml");

			controlErrorMessageItem = qobject_cast<QQuickItem*>(_controlErrorMessageComponent->create());
			if (!controlErrorMessageItem)
			{
				Log::log() << "Could not create Control Error Item!!" << std::endl;
				return;
			}
			controlErrorMessageItem->setProperty("form", QVariant::fromValue(this));
			_controlErrorMessageCache.append(controlErrorMessageItem);
		}

		QQuickItem* container = this;
		if (control->parentListView())
		{
			container = control->parentListView()->property("listGridView").value<QQuickItem*>();
			if (!container)
				container = control->parentListView();
		}

		controlErrorMessageItem->setProperty("control", QVariant::fromValue(control));
		controlErrorMessageItem->setProperty("warning", warning);
		controlErrorMessageItem->setParentItem(container);
		QMetaObject::invokeMethod(controlErrorMessageItem, "showMessage", Qt::QueuedConnection, Q_ARG(QVariant, message), Q_ARG(QVariant, temporary));
	}

	if (warning)	control->setHasWarning(true);
	else			control->setHasError(true);
}

bool AnalysisForm::hasError()
{
	// _controls have only controls created when the form is created, not the ones created dynamically afterwards
	// So here we use a workaround: check whether one errorMessage item in _controlErrorMessageCache has a control (do not use visible since it becomes visible too late).
	// Controls handling inside a form must indeed be done in anther way!

	for (QQuickItem* item : _controlErrorMessageCache)
		if (item->property("control").value<JASPControl*>() != nullptr)
			return true;

	return false;
}

void AnalysisForm::clearControlError(JASPControl* control)
{
	if (!control) return;

	for (QQuickItem* errorItem : _controlErrorMessageCache)
	{
		JASPControl* errorControl = errorItem->property("control").value<JASPControl*>();
		if (errorControl == control)
			errorItem->setProperty("control", QVariant());
	}

	control->setHasError(false);
	control->setHasWarning(false);
}

void AnalysisForm::clearFormErrors()
{
	_formErrors.clear();
	emit errorsChanged();
}


void AnalysisForm::clearFormWarnings()
{
	_formWarnings.clear();
	emit warningsChanged();

	for(auto & control : _controls)
		control->setHasWarning(false);
}

void AnalysisForm::setAnalysis(QVariant analysis)
{
	Analysis * analysisPointer = qobject_cast<Analysis *>(analysis.value<QObject *>());

	if(_analysis == analysisPointer) return;

	if(_analysis && analysisPointer)
		throw std::runtime_error("An analysis of an analysisform was replaced by another analysis, this is decidedly NOT supported!");

	_analysis = analysisPointer;

	setAnalysisUp();
}

void AnalysisForm::rSourceChanged(const QString &name)
{
	if (_rSourceModelMap.contains(name))
	{
		for (ListModel* model : _rSourceModelMap[name])
			model->sourceTermsReset();
	}
}

void AnalysisForm::boundValueChangedHandler(JASPControl *)
{
	if (_signalValueChangedBlocked == 0 && _analysis)
		_analysis->boundValueChangedHandler();
}


void AnalysisForm::formCompletedHandler()  { QTimer::singleShot(0, this, &AnalysisForm::_formCompletedHandler); }
void AnalysisForm::_formCompletedHandler()
{
	_formCompleted = true;
	setAnalysisUp();
}

void AnalysisForm::setAnalysisUp()
{
	if(!_formCompleted || !_analysis)
		return;

	connect(_analysis, &Analysis::hasVolatileNotesChanged,	this, &AnalysisForm::hasVolatileNotesChanged);
	connect(_analysis, &Analysis::needsRefreshChanged,		this, &AnalysisForm::needsRefreshChanged	);

	bool isNewAnalysis = _analysis->optionsFromJASPFile().size() == 0;

	blockValueChangeSignal(true);

	_setUpControls();
	bindTo();

	blockValueChangeSignal(false, false);

	_analysis->initialized(this, isNewAnalysis);
	_initialized = true;

	emit analysisChanged();
}

void AnalysisForm::knownIssuesUpdated()
{
	if(!_formCompleted || !_analysis)
		return;

	if(KnownIssues::issues()->hasIssues(_analysis->module(), _analysis->name()))
	{
		const std::vector<KnownIssues::issue> & issues = KnownIssues::issues()->getIssues(_analysis->module(), _analysis->name());

		for(const KnownIssues::issue & issue : issues)
		{
			for(const std::string & option : issue.options)
				if(_controls.count(tq(option)) > 0)
					_controls[tq(option)]->setHasWarning(true);

			_formWarnings.append(tq(issue.info));
		}

		emit warningsChanged();
	}
}

void AnalysisForm::setControlIsDependency(QString controlName, bool isDependency)
{
	if(_controls.count(controlName) > 0)
		_controls[controlName]->setProperty("isDependency", isDependency);
}

void AnalysisForm::setControlMustContain(QString controlName, QStringList containThis)
{
	if(_controls.count(controlName) > 0)
		_controls[controlName]->setProperty("dependencyMustContain", containThis);
}

void AnalysisForm::setMustBe(std::set<std::string> mustBe)
{
	for(const std::string & mustveBeen : _mustBe)
		if(mustBe.count(mustveBeen) == 0)
			setControlIsDependency(mustveBeen, false);

	_mustBe = mustBe;

	for(const std::string & mustBecome : _mustBe)
		setControlIsDependency(mustBecome, true); //Its ok if it does it twice, others will only be notified on change
}

void AnalysisForm::setMustContain(std::map<std::string,std::set<std::string>> mustContain)
{
	//For now ignore specific thing that must be contained
	for(const auto & nameContainsPair : _mustContain)
		if(mustContain.count(nameContainsPair.first) == 0)
			setControlMustContain(nameContainsPair.first, {});

	_mustContain = mustContain;

	for(const auto & nameContainsPair : _mustContain)
		setControlMustContain(nameContainsPair.first, nameContainsPair.second); //Its ok if it does it twice, others will only be notified on change

}

void AnalysisForm::setRunOnChange(bool change)
{
	if (change != _runOnChange)
	{
		_runOnChange = change;

		blockValueChangeSignal(change, false);

		emit runOnChangeChanged();
	}
}

void AnalysisForm::blockValueChangeSignal(bool block, bool notifyOnceUnblocked)
{
	if (block)
		_signalValueChangedBlocked++;
	else
	{
		_signalValueChangedBlocked--;
		if (_signalValueChangedBlocked < 0)	_signalValueChangedBlocked = 0;

		if (_signalValueChangedBlocked == 0 && notifyOnceUnblocked && _analysis)
			_analysis->boundValueChangedHandler();
	}
}

bool AnalysisForm::needsRefresh() const
{
	return _analysis ? _analysis->needsRefresh() : false;
}

bool AnalysisForm::hasVolatileNotes() const
{
	return _analysis ? _analysis->hasVolatileNotes() : false;
}

QString AnalysisForm::metaHelpMD() const
{
	std::function<QString(const Json::Value & meta, int deep)> metaMDer = [&metaMDer](const Json::Value & meta, int deep)
	{
		QStringList markdown;

		for(const Json::Value & entry : meta)
		{
			std::string entryType	= entry.get("type", "").asString();
			//Sadly enough the following "meta-types" aren't defined properly anywhere, this would be good to do at some point. The types are: table, image, collection, and optionally: htmlNode, column, json
			QString friendlyObject	= entryType == "table"		? tr("Table")
									: entryType == "image"		? tr("Plot")
									: entryType == "collection"	? tr("Collection")
									: tr("Result"); //Anything else we just call "Result"

			if(entry.get("info", "") != "")
			{
				for(int i=0; i<deep; i++) markdown << "#";
				markdown << " " << friendlyObject;
				if(entry.get("title", "") != "")	markdown << tq(" - *" + entry["title"].asString() + "*:\n");
				else								markdown << "\n";
				markdown << tq(entry["info"].asString() + "\n");
			}

			if(entry.get("meta", Json::nullValue).isArray())
				markdown << "\n" << metaMDer(entry["meta"], deep + 1);
		}

		return markdown.join("");
	};

	return "---\n# " + tr("Output") + "\n\n" + metaMDer(_analysis->meta(), 2);
}

std::vector<std::vector<string> > AnalysisForm::getValuesFromRSource(const QString &sourceID, const QStringList &searchPath)
{
	if (!_analysis) return {};

	const Json::Value& jsonSource = _analysis->getRSource(fq(sourceID));
	return  _getValuesFromJson(jsonSource, searchPath);
}

std::vector<std::vector<string> > AnalysisForm::_getValuesFromJson(const Json::Value& jsonValues, const QStringList& searchPath)
{
	auto getValueFromJson = [](const Json::Value& jsonValue) -> std::vector<std::string>
	{
		if (jsonValue.isString())			return {jsonValue.asString()};
		else if (jsonValue.isIntegral())	return {std::to_string(jsonValue.asInt())};
		else if (jsonValue.isNumeric())		return {std::to_string(jsonValue.asDouble())};
		else if (jsonValue.isArray())
		{
			std::vector<std::string> values;
			for (const Json::Value& oneValue: jsonValue)
			{
				if (oneValue.isString())	values.push_back(oneValue.asString());
				if (oneValue.isIntegral())	values.push_back(std::to_string(oneValue.asInt()));
				if (oneValue.isNumeric())	values.push_back(std::to_string(oneValue.asDouble()));
			}
			return values;
		}
		else return {};
	};

	if (!jsonValues.isArray() && !jsonValues.isObject())
		return {getValueFromJson(jsonValues)};

	std::vector<std::vector<string> > result;
	QString path;
	QStringList nextPaths;
	if (searchPath.length() > 0)
	{
		path = searchPath[0];
		nextPaths = searchPath;
		nextPaths.removeAt(0);
	}

	if (jsonValues.isObject())
	{
		if (path.isEmpty())
		{
			std::vector<std::string> keys = jsonValues.getMemberNames();
			for (const std::string& key : keys)
				result.push_back({key});
		}
		else
		{
			std::string key = fq(path);
			if (jsonValues.isMember(key))
				result = _getValuesFromJson(jsonValues[key], nextPaths);
			else if (key == "values")
			{
				for (const Json::Value& jsonValue : jsonValues)
				{
					std::vector<std::vector<string> > values = _getValuesFromJson(jsonValue, nextPaths);
					if (values.size() > 0)
						result.push_back(values[0]);
				}
			}
			else
				Log::log() << "Key " << key << " not found in R source " << jsonValues.toStyledString() << std::endl;
		}
	}
	else // jsonValues is an array
	{
		bool pathIsIndex = false;
		uint index = path.isEmpty() ? 0 : path.toUInt(&pathIsIndex);

		if (pathIsIndex)
		{
			if (jsonValues.size() > index)
				result = _getValuesFromJson(jsonValues[index], nextPaths);
			else
				Log::log() << "Cannot retrieve values from R Source: index (" << index << ") bigger than size of the source (" << jsonValues.size() << ")" << std::endl;
		}
		else
		{
			for (const Json::Value& jsonValue : jsonValues)
			{
				if (path.isEmpty())
					result.push_back(getValueFromJson(jsonValue));
				else if (jsonValue.isObject())
				{
					std::string key = fq(path);
					if (jsonValue.isMember(key))
					{
						std::vector<std::vector<string> > values =_getValuesFromJson(jsonValue[key], nextPaths);
						if (values.size() > 0)
							result.push_back(values[0]);
					}
					else
						Log::log() << "Key " << key << " not found in R source " << jsonValue.toStyledString() << std::endl;
				}
				else
					Log::log() << "Caanot find path " << path << " in R source " << jsonValue.toStyledString() << std::endl;
			}
		}
	}

	return result;
}

void AnalysisForm::setBoundValue(const string &name, const Json::Value &value, const Json::Value &meta, const QVector<JASPControl::ParentKey> &parentKeys)
{
	if (_analysis)
		_analysis->setBoundValue(name, value, meta, parentKeys);
}

std::set<string> AnalysisForm::usedVariables()
{
	std::set<string> result;

	for (JASPControl* control : _controls)
	{
		BoundControl* boundControl = control->boundControl();
		if (boundControl)
		{
			std::vector<std::string> usedVariables = boundControl->usedVariables();
			result.insert(usedVariables.begin(), usedVariables.end());
		}
	}

	return result;
}

QString AnalysisForm::helpMD() const
{
	if(!_analysis) return "";

	QStringList markdown =
	{
		_analysis->titleQ(), "\n",
		"=====================\n",
		_info, "\n\n",
		"---\n# ", tr("Input"), "\n"
	};

	QList<JASPControl*> orderedControls = JASPControl::getChildJASPControls(this);

	for(JASPControl * control : orderedControls)
		markdown.push_back(control->helpMD());

	markdown.push_back(metaHelpMD());

	return markdown.join("");
}
