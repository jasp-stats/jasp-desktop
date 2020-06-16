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


#include <boost/bind.hpp>

#include "options/bound.h"
#include "utilities/qutils.h"

#include <QQmlProperty>
#include <QQmlContext>

#include "widgets/listmodeltermsavailable.h"

#include "utils.h"
#include "dirs.h"
#include "utilities/settings.h"
#include "gui/messageforwarder.h"
#include "mainwindow.h"
#include "log.h"
#include "jaspcontrolbase.h"
#include "widgets/boundqmlcombobox.h"
#include "widgets/boundqmltextarea.h"
#include "widgets/boundqmllistviewterms.h"
#include "widgets/boundqmllistviewmeasurescells.h"
#include "widgets/boundqmllistviewlayers.h"
#include "widgets/boundqmlinputlist.h"
#include "widgets/boundqmlrepeatedmeasuresfactors.h"
#include "widgets/boundqmlfactorsform.h"
#include "widgets/boundqmltableview.h"
#include "widgets/qmllistviewtermsavailable.h"
#include "widgets/qmlexpander.h"


using namespace std;

AnalysisForm::AnalysisForm(QQuickItem *parent) : QQuickItem(parent)
{
	setObjectName("AnalysisForm");

	connect(this, &AnalysisForm::formCompleted, this, &AnalysisForm::formCompletedHandler);
}

QVariant AnalysisForm::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	try {
		switch(info)
		{
		case VariableInfo::VariableType:		return int(					DataSetPackage::pkg()->getColumnType(term.asString()));
		case VariableInfo::VariableTypeName:	return columnTypeToQString(	DataSetPackage::pkg()->getColumnType(term.asString()));
		case VariableInfo::Labels:				return						DataSetPackage::pkg()->getColumnLabelsAsStringList(term.asString());
		}
	}
	catch(columnNotFound & e) {} //just return an empty QVariant right?
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}
	return QVariant();

}

void AnalysisForm::runRScript(QString script, QString controlName, bool whiteListedVersion)
{
	if(_analysis && !_removed)
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
		for (JASPControlWrapper* control : _orderedControls)
			// controls will be automatically deleted by the deletion of AnalysisForm
			// But they must be first disconnected: sometimes an event seems to be triggered before the item is completely destroyed
			control->cleanUp();
	}
}

void AnalysisForm::runScriptRequestDone(const QString& result, const QString& controlName)
{	
	if(_removed)
		return;

	BoundQMLItem* item = dynamic_cast<BoundQMLItem*>(getControl(controlName));
	if (!item)
	{
		QStringList composedName = controlName.split(".");
		if (composedName.length() == 3)
		{
			JASPControlWrapper* parentControl = getControl(composedName[0]);
			if (parentControl)
				item = dynamic_cast<BoundQMLItem*>(parentControl->getChildControl(composedName[1], composedName[2]));
		}
	}

	if (item)
		item->rScriptDoneHandler(result);
	else
		Log::log() << "Unknown item " << controlName.toStdString() << std::endl;
}

void AnalysisForm::_addControlWrapper(JASPControlWrapper* controlWrapper)
{
	switch(controlWrapper->item()->controlType())
	{
	case JASPControlBase::ControlType::Expander:
	{
		QMLExpander* expander = dynamic_cast<QMLExpander*>(controlWrapper);
		_expanders.push_back(expander);
		break;
	}
	case JASPControlBase::ControlType::TextArea:
	{
		BoundQMLTextArea* boundQMLTextArea = dynamic_cast<BoundQMLTextArea*>(controlWrapper);
		ListModelTermsAvailable* availableModel = dynamic_cast<ListModelTermsAvailable*>(boundQMLTextArea->model());
		if (availableModel)
		{
			if (boundQMLTextArea->modelHasAllVariables())
				_allAvailableVariablesModels.push_back(availableModel);
			_modelMap[controlWrapper->name()] = availableModel;
		}

		break;
	}
	case JASPControlBase::ControlType::ComboBox:
	{
		BoundQMLComboBox* boundQMLComboBox = dynamic_cast<BoundQMLComboBox*>(controlWrapper);
		ListModelTermsAvailable* availableModel = dynamic_cast<ListModelTermsAvailable*>(boundQMLComboBox->model());
		if (availableModel)
		{
			if (boundQMLComboBox->modelHasAllVariables())
				_allAvailableVariablesModels.push_back(availableModel);
			_modelMap[controlWrapper->name()] = availableModel;
		}
		break;
	}
	case JASPControlBase::ControlType::RepeatedMeasuresFactorsList:
	case JASPControlBase::ControlType::InputListView:
	case JASPControlBase::ControlType::TabView:
	case JASPControlBase::ControlType::ComponentsList:
	case JASPControlBase::ControlType::FactorsForm:
	case JASPControlBase::ControlType::TableView:
	case JASPControlBase::ControlType::VariablesListView:
	{
		QMLListView* listView = dynamic_cast<QMLListView*>(controlWrapper);
		_modelMap[controlWrapper->name()] = listView->model();

		QMLListViewTermsAvailable* listViewTermsAvailable = dynamic_cast<QMLListViewTermsAvailable*>(listView);
		if (listViewTermsAvailable)
		{
			ListModelTermsAvailable* availableModel = dynamic_cast<ListModelTermsAvailable*>(listViewTermsAvailable->model());

			if (availableModel)
			{
				if (!listViewTermsAvailable->hasSource())
					_allAvailableVariablesModels.push_back(availableModel);
				else
					_allAvailableVariablesModelsWithSource.push_back(availableModel);
			}
		}
		break;
	}
	default:
		break;
	}

}

void AnalysisForm::addControl(JASPControlBase *control)
{
	if (control->isBound())
	{
		const QString& name = control->name();
		if (name.isEmpty())
		{
			QString label = control->property("label").toString();
			if (label.isEmpty())
				label = control->property("title").toString();

			if (label.isEmpty())	_formErrorMessages.append(tr("Control with label '%1' has no name").arg(label));
			else					_formErrorMessages.append(tr("A control has no name"));
		}
		else if (_controls.keys().contains(name))
			_formErrorMessages.append(tr("2 controls have the same name: %1").arg(name));
		else
		{
			JASPControlWrapper* wrapper = control->getWrapper();
			_controls[name] = wrapper;
			_addControlWrapper(wrapper);
		}
	}
	else
		_addControlWrapper(control->getWrapper());
}

void AnalysisForm::_setUpControls()
{
	_analysis->setUsesJaspResults(QQmlProperty(this, "usesJaspResults").read().toBool());

	_orderExpanders();
	_setUpRelatedModels();
	_setUpItems();

	if (!_formErrorMessagesItem)
		Log::log()  << "No errorMessages Item found!" << std::endl;

	_setErrorMessages();
}

void AnalysisForm::_setUpRelatedModels()
{
	for (ListModel* model : _modelMap.values())
	{
		QMLListView* listView = model->listView();
		QList<QVariant> dropKeyList = listView->getItemProperty("dropKeys").toList();
		QString dropKey				= dropKeyList.isEmpty() ? listView->getItemProperty("dropKeys").toString() : dropKeyList[0].toString(); // The first key gives the default drop item.

		if (!dropKey.isEmpty())
		{
			ListModel* targetModel = _modelMap[dropKey];
			if (targetModel)
				_relatedModelMap[listView] = targetModel;
			else
				_formErrorMessages.append(tr("Cannot find a source %1 for VariableList %2").arg(dropKey).arg(listView->name()));
		}
		else
		{
			bool draggable = listView->getItemProperty("draggabble").toBool();
			if (draggable)
				_formErrorMessages.append(tr("No drop key found for %1").arg(listView->name()));
		}

	}
}

void AnalysisForm::_setUpItems()
{
	QList<JASPControlWrapper*> controls = _controls.values();
	for (JASPControlWrapper* control : controls)
		control->setUp();

	// set the order of the BoundItems according to their dependencies (for binding purpose)
	for (JASPControlWrapper* control : controls)
	{
		QVector<JASPControlWrapper*> depends = control->depends();
		int index = 0;
		while (index < depends.length())
		{
			JASPControlWrapper* depend = depends[index];
			const QVector<JASPControlWrapper*>& dependdepends = depend->depends();
			for (JASPControlWrapper* dependdepend : dependdepends)
			{
				if (dependdepend == control)
					addFormError(tq("Circular dependency between control ") + control->name() + tq(" and ") + depend->name());
				else
				{
					if (control->addDependency(dependdepend))
						depends.push_back(dependdepend);
				}
			}
			index++;
		}
	}
	std::sort(controls.begin(), controls.end(), 
			  [](JASPControlWrapper* a, JASPControlWrapper* b) {
					return a->depends().length() < b->depends().length(); 
			});

	for (JASPControlWrapper* control : controls)
	{
		_orderedControls.push_back(control);
	}
}

void AnalysisForm::_orderExpanders()
{
	for (QMLExpander* expander : _expanders)
	{
		bool foundExpander = false;
		for (QObject* sibling : expander->item()->parent()->parent()->children())
		{
			if (sibling->objectName() == "Section")
			{
				QObject* button = sibling->property("button").value<QObject*>();
				JASPControlBase* control = qobject_cast<JASPControlBase*>(button);
				if (control && control->controlType() == JASPControlBase::ControlType::Expander)
				{
					if (foundExpander)
					{
						_nextExpanderMap[expander] = dynamic_cast<QMLExpander*>(control->getWrapper());
						break;
					}
					if (control->getWrapper() == expander)
						foundExpander = true;
				}
			}
		}
		expander->setUp();
	}
}

void AnalysisForm::addListView(QMLListView* listView, QMLListView* source)
{
	_modelMap[listView->name()] = listView->model();
	_relatedModelMap[listView] = source->model();
	_relatedModelMap[source] = listView->model();
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

void AnalysisForm::_setErrorMessages()
{
	if (_formErrorMessagesItem)
	{
		if (!_formErrorMessages.isEmpty())
		{
			QString text;
			if (_formErrorMessages.length() == 1)
				text = _formErrorMessages[0];
			else
			{
				text.append("<ul style=\"margin-bottom:0px\">");
				for (const QString& errorMessage : _formErrorMessages)
					text.append("<li>").append(errorMessage).append("</li>");
				text.append("</ul>");
			}
			QQmlProperty(_formErrorMessagesItem, "text").write(QVariant::fromValue(text));
			_formErrorMessagesItem->setVisible(true);
		}
		else
		{
			QQmlProperty(_formErrorMessagesItem, "text").write(QVariant::fromValue(QString()));
			_formErrorMessagesItem->setVisible(false);
		}
	}
}

void AnalysisForm::replaceVariableNameInListModels(const std::string & oldName, const std::string & newName)
{
	for (ListModelTermsAvailable * model : _allAvailableVariablesModels)
	{
		model->replaceVariableName(oldName, newName);

		QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(model->listView());
		if (qmlAvailableListView)
		{
			const QList<ListModelAssignedInterface*>& assignedModels = qmlAvailableListView->assignedModel();
			for (ListModelAssignedInterface* modelAssign : assignedModels)
				    modelAssign->replaceVariableName(oldName, newName);
		}
	}
}


void AnalysisForm::_setAllAvailableVariablesModel(bool refreshAssigned)
{
	if (_allAvailableVariablesModels.size() == 0)
		return;

	std::vector<std::string> columnNames = DataSetPackage::pkg()->getColumnNames();


	for (ListModelTermsAvailable* model : _allAvailableVariablesModels)
	{
		model->initTerms(columnNames);

		if (refreshAssigned)
		{
			emit model->allAvailableTermsChanged(nullptr, nullptr);
			QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(model->listView());
			if (qmlAvailableListView)
			{
				const QList<ListModelAssignedInterface*>& assignedModels = qmlAvailableListView->assignedModel();	
				for (ListModelAssignedInterface* modelAssign : assignedModels)
					modelAssign->refresh();
			}
		}
	}

	if (refreshAssigned)
		for (ListModelTermsAvailable * model : _allAvailableVariablesModelsWithSource)
			model->resetTermsFromSourceModels(true);

}

QString AnalysisForm::_getControlLabel(JASPControlBase* control)
{
	QString label = control->property("label").toString();

	if (label.isEmpty())
		label = control->property("title").toString();

	if (label.isEmpty())
		label = control->name();

	label = label.simplified();
	if (label.right(1) == ":")
		label = label.chopped(1);

	return label;
}

void AnalysisForm::_addLoadingError()
{
	if (_jaspControlsWithWarningSet.size() > 0)
	{
		QString errorMsg;
		if (_jaspControlsWithWarningSet.size() == 1)
		{
			JASPControlBase* control = _jaspControlsWithWarningSet.values()[0];
			errorMsg = tr("Component %1 was loaded with the wrong type of value and has been reset to its default value.").arg(control->name());
			errorMsg += "<br>";
		}
		else if (_jaspControlsWithWarningSet.size() < 4)
		{
			QString names = "<ul>";
			QSetIterator<JASPControlBase *> it(_jaspControlsWithWarningSet);
			while (it.hasNext())
				names += "<li>" + _getControlLabel(it.next()) + "</li>";
			names += "</ul>";

			errorMsg = tr("These components were loaded with a wrong type of value and have been reset to their default values:%1").arg(names);
		}
		else
		{
			errorMsg = tr("Many components were loaded with a wrong type of value and have been reset to their default values.");
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
	if (_options != nullptr)
		unbind();

	const Json::Value& optionsFromJASPFile = _analysis->optionsFromJASPFile();
	_options = _analysis->options();
	QVector<ListModelAvailableInterface*> availableModelsToBeReset;

	_options->blockSignals(true);
	
	_setAllAvailableVariablesModel();	
	
	for (JASPControlWrapper* control : _orderedControls)
	{
		BoundQMLItem* boundControl = dynamic_cast<BoundQMLItem*>(control);
		if (boundControl)
		{
			std::string name = boundControl->name().toStdString();
			Option* option   = _options->get(name);

			if (option && !boundControl->isOptionValid(option))
			{
				option = nullptr;
				boundControl->item()->setHasWarning(true);
			}

			if (!option && optionsFromJASPFile != Json::nullValue)
			{
				const Json::Value& optionValue = optionsFromJASPFile[name];
				if (optionValue != Json::nullValue)
				{
					if (!boundControl->isJsonValid(optionValue))
						boundControl->item()->setHasWarning(true);
					else
					{
						// call createOption after checking Json options in isJsonValid: if Json comes from an older version of JASP, createOption can take care of backward compatibility issues.
						option = boundControl->createOption();
						option->set(optionValue);
						_options->add(name, option);
					}
				}
			}

			if (!option)
			{
				option = boundControl->createOption();
				_options->add(name, option);
			}

			boundControl->bindTo(option);
			_optionControlMap[option] = boundControl;
		}
		else
		{
			QMLListViewTermsAvailable* availableListControl = dynamic_cast<QMLListViewTermsAvailable *>(control);
			if (availableListControl && availableListControl->hasSource())
			{
				ListModelAvailableInterface* availableModel = availableListControl->availableModel();
				// The availableList control are not bound with options, but they have to be updated from their sources when the form is initialized.
				// The availableList cannot signal their assigned models now because they are not yet bound (the controls are ordered by dependency)
				// When the options come from a JASP file, an assigned model needs sometimes the available model (eg. to determine the kind of terms they have).
				// So in this case resetTermsFromSourceModels has to be called now but with updateAssigned argument set to false.
				// When the options come from default options (from source models), the availableList needs sometimes to signal their assigned models (e.g. when addAvailableVariablesToAssigned if true).
				// As their assigned models are not yet bound, resetTermsFromSourceModels (with updateAssigned argument set to true) must be called afterwards.
				if (availableModel)
				{
					if (optionsFromJASPFile != Json::nullValue || _analysis->isDuplicate())
						availableModel->resetTermsFromSourceModels(false);
					else
						availableModelsToBeReset.push_back(availableModel);
				}
			}
		}
	}

	for (ListModelAvailableInterface* availableModel : availableModelsToBeReset)
		availableModel->resetTermsFromSourceModels(true);
	
	_addLoadingError();

	_options->blockSignals(false, false);

	//Ok we can only set the warnings on the components now, because otherwise _addLoadingError() will add a big fat red warning on top of the analysisform without reason...
	for (JASPControlWrapper* control : _orderedControls)
	{
		QString upgradeMsg(tq(_analysis->upgradeMsgsForOption(fq(control->name()))));

		if(upgradeMsg != "")
			control->item()->addControlWarning(upgradeMsg);
	}

	//Also check for a warning to show above the analysis:
	QString upgradeMsg(tq(_analysis->upgradeMsgsForOption("")));

	if(upgradeMsg != "")
		addFormError(upgradeMsg);

}

void AnalysisForm::unbind()
{
	if (_options == nullptr)
		return;
	
	for (JASPControlWrapper* control : _orderedControls)
	{
		BoundQMLItem* boundControl = dynamic_cast<BoundQMLItem*>(control);
		if (boundControl)
			boundControl->unbind();
	}

	_options = nullptr;
}

void AnalysisForm::addFormError(const QString &error)
{
	_formErrorMessages.append(error);
	_lastAddedErrorTimestamp = Utils::currentSeconds();
	_setErrorMessages();
}

QQuickItem* AnalysisForm::_getControlErrorMessageUsingThisJaspControl(JASPControlBase* jaspControl)
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

void AnalysisForm::addControlError(JASPControlBase* control, QString message, bool temporary, bool warning)
{
	if (!control) return;

	if (!message.isEmpty())
	{
		QQuickItem*	controlErrorMessageItem = nullptr;

		for (QQuickItem* item : _controlErrorMessageCache)
		{
			JASPControlBase* errorControl = item->property("control").value<JASPControlBase*>();
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

	if (warning)
		control->setHasWarning(true);
	else
		control->setHasError(true);
}

void AnalysisForm::addControlErrorSet(JASPControlBase *control, bool add)
{
	if (!control) return;

	if (add)	_jaspControlsWithErrorSet.insert(control);
	else		_jaspControlsWithErrorSet.remove(control);
}

void AnalysisForm::addControlWarningSet(JASPControlBase *control, bool add)
{
	if (!control) return;

	if (add)	_jaspControlsWithWarningSet.insert(control);
	else		_jaspControlsWithWarningSet.remove(control);
}

void AnalysisForm::clearControlError(JASPControlBase* control)
{
	if (!control) return;

	for (QQuickItem* errorItem : _controlErrorMessageCache)
	{
		JASPControlBase* errorControl = errorItem->property("control").value<JASPControlBase*>();
		if (errorControl == control)
			errorItem->setProperty("control", QVariant());
	}

	control->setHasError(false);
	control->setHasWarning(false);
}

void AnalysisForm::clearFormErrors()
{
	if (Utils::currentSeconds() - _lastAddedErrorTimestamp > 5)
	{
		_formErrorMessages.clear();
		_setErrorMessages();

		// Remove also warning without text
		QSet<JASPControlBase*> controlsWithWarning = _jaspControlsWithWarningSet; // Copy the set: the setHasWarning method changes it.

		for (JASPControlBase* control : controlsWithWarning)
		{
			if (!_getControlErrorMessageUsingThisJaspControl(control))
				control->setHasWarning(false);
		}
	}
}

void AnalysisForm::formCompletedHandler()
{
	QTimer::singleShot(0, this, &AnalysisForm::_formCompletedHandler);
}

void AnalysisForm::_formCompletedHandler()
{
	QVariant analysisVariant = QQmlProperty(this, "analysis").read();
	if (!analysisVariant.isNull())
	{
		_analysis	= qobject_cast<Analysis *>(analysisVariant.value<QObject *>());

		connect(_analysis, &Analysis::hasVolatileNotesChanged,	this, &AnalysisForm::hasVolatileNotesChanged);
		connect(_analysis, &Analysis::needsRefreshChanged,		this, &AnalysisForm::needsRefreshChanged	);

		_setUpControls();

		bool isNewAnalysis = _analysis->options()->size() == 0 && _analysis->optionsFromJASPFile().size() == 0;

		bindTo();

		_analysis->resetOptionsFromJASPFile();
		_analysis->initialized(this, isNewAnalysis);
	}
}

void AnalysisForm::dataSetChangedHandler()
{
	if (!_removed && DataSetPackage::pkg() && DataSetPackage::pkg()->hasDataSet())
	{
		_setAllAvailableVariablesModel(true);
		emit dataSetChanged();
	}
}

void AnalysisForm::dataSetColumnsChangedHandler()
{
	if (!_removed && DataSetPackage::pkg() && DataSetPackage::pkg()->hasDataSet())
	{
		for (ListModel* model : _modelMap.values())
			model->refresh();

		emit dataSetChanged();
	}
}

void AnalysisForm::setControlIsDependency(QString controlName, bool isDependency)
{
	if(_controls.count(controlName) > 0)
		_controls[controlName]->setItemProperty("isDependency", isDependency);
}

void AnalysisForm::setControlMustContain(QString controlName, QStringList containThis)
{
	if(_controls.count(controlName) > 0)
		_controls[controlName]->setItemProperty("dependencyMustContain", containThis);
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

		if (_options)
			_options->blockSignals(change, false);

		emit runOnChangeChanged();
	}
}

bool AnalysisForm::runWhenThisOptionIsChanged(Option *option)
{
	BoundQMLItem* control = getBoundItem(option);
	JASPControlBase* item = control ? control->item() : nullptr;

	emit valueChanged(item);
	if (item)
		emit item->valueChanged();

	if (!_runOnChange)
		return false;

	if (item && !item->runOnChange())
		return false;

	return true;
}


bool AnalysisForm::needsRefresh() const
{
	return _analysis ? _analysis->needsRefresh() : false;
}

bool AnalysisForm::hasVolatileNotes() const
{
	return _analysis ? _analysis->hasVolatileNotes() : false;
}
