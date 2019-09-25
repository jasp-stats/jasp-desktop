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

#include "widgets/boundqmlcheckbox.h"
#include "widgets/boundqmlcombobox.h"
#include "widgets/boundqmlslider.h"
#include "widgets/boundqmltextinput.h"
#include "widgets/boundqmltextarea.h"
#include "widgets/boundqmlradiobuttons.h"
#include "widgets/boundqmllistviewpairs.h"
#include "widgets/boundqmllistviewterms.h"
#include "widgets/boundqmllistviewmeasurescells.h"
#include "widgets/boundqmllistviewlayers.h"
#include "widgets/boundqmlrepeatedmeasuresfactors.h"
#include "widgets/boundqmlfactorsform.h"
#include "widgets/boundqmltableview.h"
#include "widgets/qmllistviewtermsavailable.h"
#include "widgets/listmodeltermsavailable.h"

#include "utilities/qutils.h"
#include "utils.h"
#include "dirs.h"
#include "utilities/settings.h"
#include "gui/messageforwarder.h"
#include "mainwindow.h"
#include "log.h"

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
		case VariableInfo::VariableType:		return int(_package->getColumnType(term.asString()));
		case VariableInfo::VariableTypeName:	return columnTypeToQString(_package->getColumnType(term.asString()));
		case VariableInfo::Labels:				return _package->getColumnLabelsAsStringList(term.asString());
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
	if(_analysis != nullptr && !_removed)
		emit _analysis->sendRScript(_analysis, script, controlName, whiteListedVersion);
}

void AnalysisForm::refreshAnalysis()
{
	_analysis->refresh();
}

void AnalysisForm::itemChange(QQuickItem::ItemChange change, const QQuickItem::ItemChangeData &value)
{
	if (change == ItemChange::ItemSceneChange && !value.window)
		_cleanUpForm();
	QQuickItem::itemChange(change, value);
}

void AnalysisForm::_cleanUpForm()
{
	_removed = true;
	for (QMLItem* control : _orderedControls)
		// controls will be automatically deleted by the deletion of AnalysisForm
		// But they must be first disconnected: sometimes an event seems to be triggered before the item is completely destroyed
		control->cleanUp();
}

void AnalysisForm::runScriptRequestDone(const QString& result, const QString& controlName)
{	
	if(_removed)
		return;

	BoundQMLItem* item = dynamic_cast<BoundQMLItem*>(getControl(controlName));

	if (item)
		item->rScriptDoneHandler(result);
	else
		Log::log() << "Unknown item " << controlName.toStdString() << std::endl;
}

void AnalysisForm::_parseQML()
{
	QQuickItem *root = this;

	_analysis->setUsesJaspResults(QQmlProperty(root, "usesJaspResults").read().toBool());

	map<QString, QString>	dropKeyMap;
	QList<QString>			controlNames;

	for (QQuickItem* quickItem : root->findChildren<QQuickItem *>())
	{
		if (quickItem->objectName() == "errorMessagesBox")
		{
			_errorMessagesItem = quickItem;
			continue;
		}

		QString controlTypeStr = QQmlProperty(quickItem, "controlType").read().toString();
		if (controlTypeStr.isEmpty())
			continue;

		if (! QQmlProperty(quickItem, "isBound").read().toBool())
			continue;

		QString controlName = QQmlProperty(quickItem, "name").read().toString();

		if (controlName.isEmpty())
		{
			_errorMessages.append(QString::fromLatin1("A control ") + controlTypeStr + QString::fromLatin1(" has no name"));
			continue;
		}
		if (controlNames.contains(controlName))
		{
			_errorMessages.append(QString::fromLatin1("2 controls have the same name: ") + controlName);
			continue;
		}
		controlNames.append(controlName);

		QMLItem *control = nullptr;
		qmlControlType controlType;
		try						{ controlType	= qmlControlTypeFromQString(controlTypeStr);	}
		catch(std::exception&)	{ _errorMessages.append(QString::fromLatin1("Unknown Control type: ") + controlTypeStr); continue; }

		switch(controlType)
		{
		case qmlControlType::CheckBox:			[[clang::fallthrough]];
		case qmlControlType::Switch:			control = new BoundQMLCheckBox(quickItem,		this);	break;
		case qmlControlType::TextField:			control = new BoundQMLTextInput(quickItem,		this);	break;
		case qmlControlType::RadioButtonGroup:	control = new BoundQMLRadioButtons(quickItem,	this);	break;
		case qmlControlType::Slider:			control = new BoundQMLSlider(quickItem,			this);	break;
		case qmlControlType::TextArea:
		{
			BoundQMLTextArea* boundQMLTextArea = new BoundQMLTextArea(quickItem,	this);
			control = boundQMLTextArea;
			ListModelTermsAvailable* allVariablesModel = boundQMLTextArea->allVariablesModel();

			if (allVariablesModel)
				_allAvailableVariablesModels.push_back(allVariablesModel);

			break;
		}
		case qmlControlType::ComboBox:
		{
			BoundQMLComboBox* boundQMLComboBox = new BoundQMLComboBox(quickItem, this);
			control = boundQMLComboBox;
			ListModelTermsAvailable* availableModel = dynamic_cast<ListModelTermsAvailable*>(boundQMLComboBox->model());
			if (availableModel)
			{
				if (boundQMLComboBox->hasAllVariablesModel)
					_allAvailableVariablesModels.push_back(availableModel);
			}
			break;
		}
		case qmlControlType::RepeatedMeasuresFactorsList:
		{
			BoundQMLRepeatedMeasuresFactors* factorList = new BoundQMLRepeatedMeasuresFactors(quickItem, this);
			control = factorList;
			_modelMap[controlName] = factorList->model();
			break;
		}
		case qmlControlType::FactorsForm:
		{
			BoundQMLFactorsForm* factorForm = new BoundQMLFactorsForm(quickItem, this);
			control = factorForm;
			_modelMap[controlName] = factorForm->model();
			break;
		}
		case qmlControlType::TableView:
		{
			BoundQMLTableView* tableView = new BoundQMLTableView(quickItem, this);
			control = tableView;
			break;
		}
		case qmlControlType::VariablesListView:
		{
			QMLListView* listView = nullptr;
			QString			listViewTypeStr = QQmlProperty(quickItem, "listViewType").read().toString();
			qmlListViewType	listViewType;

			try	{ listViewType	= qmlListViewTypeFromQString(listViewTypeStr);	}
			catch(std::exception&)
			{
				_errorMessages.append(QString::fromLatin1("Unknown listViewType: ") + listViewType + QString::fromLatin1(" form VariablesList ") + controlName);
				listViewType = qmlListViewType::AssignedVariables;
			}

			switch(listViewType)
			{
			case qmlListViewType::AssignedVariables:	listView = new BoundQMLListViewTerms(quickItem, this, false);		break;
			case qmlListViewType::Interaction:			listView = new BoundQMLListViewTerms(quickItem, this, true);		break;
			case qmlListViewType::Pairs:				listView = new BoundQMLListViewPairs(quickItem,this);				break;
			case qmlListViewType::RepeatedMeasures:		listView = new BoundQMLListViewMeasuresCells(quickItem, this);		break;
			case qmlListViewType::Layers:				listView = new BoundQMLListViewLayers(quickItem, this);				break;
			case qmlListViewType::AvailableVariables:
			case qmlListViewType::AvailableInteraction:
			{
				QMLListViewTermsAvailable* availableVariablesListView = new QMLListViewTermsAvailable(quickItem, this, listViewType == qmlListViewType::AvailableInteraction);
				listView = availableVariablesListView;

				bool noSource = availableVariablesListView->sourceModels().isEmpty(); // If there is no sourceModels, set all available variables.

				ListModelTermsAvailable* availableModel = dynamic_cast<ListModelTermsAvailable*>(availableVariablesListView->model());

				if (availableModel)
					(noSource ? _allAvailableVariablesModels : _allAvailableVariablesModelsWithSource).push_back(availableModel);

				break;
			}
			default:
				_errorMessages.append(QString::fromLatin1("Unused (in AnalysisForm::_parseQML) listViewType: ") + qmlListViewTypeToQString(listViewType) + QString::fromLatin1(". Cannot set a model to the VariablesList"));
				break;
			}

			_modelMap[controlName] = listView->model();
			control = dynamic_cast<QMLItem*>(listView);

			QList<QVariant> dropKeyList = QQmlProperty(quickItem, "dropKeys").read().toList();
			QString dropKey				= dropKeyList.isEmpty() ? QQmlProperty(quickItem, "dropKeys").read().toString() : dropKeyList[0].toString(); // The first key gives the default drop item.

			if (!dropKey.isEmpty())
				dropKeyMap[controlName] = dropKey;
			else
			{
				bool draggable = QQmlProperty(quickItem, "draggabble").read().toBool();
				if (draggable)
					_errorMessages.append(QString::fromLatin1("No drop key found for ") + controlName);
			}

			break;
		}
		case qmlControlType::JASPControl:
		default:
			_errorMessages.append(QString::fromLatin1("Unknown type of JASPControl ") + controlName + QString::fromLatin1(" : ") + controlTypeStr);
		}

		if (control)
			_controls[control->name()] = control;
	}

	_setUpRelatedModels(dropKeyMap);
	_setUpItems();

	if (!_errorMessagesItem)
		Log::log()  << "No errorMessages Item found!" << std::endl;

	_setErrorMessages();
}

void AnalysisForm::_setUpRelatedModels(const map<QString, QString>& relationMap)
{
	for (auto const& pair : relationMap)
	{
		ListModel* sourceModel = _modelMap[pair.first];
		ListModel* targetModel = _modelMap[pair.second];

		if (sourceModel && targetModel)
		{
			QMLListView* sourceListView = sourceModel->listView();
			_relatedModelMap[sourceListView] = targetModel;
		}
		else
			_errorMessages.append(QString::fromLatin1("Cannot find a ListView for ") + (!sourceModel ? pair.first : pair.second));
	}	
}

void AnalysisForm::_setUpItems()
{
	QList<QMLItem*> controls = _controls.values();
	for (QMLItem* control : controls)
		control->setUp();

	// set the order of the BoundItems according to their dependencies (for binding purpose)
	for (QMLItem* control : controls)
	{
		QVector<QMLItem*> depends = control->depends();
		int index = 0;
		while (index < depends.length())
		{
			QMLItem* depend = depends[index];
			const QVector<QMLItem*>& dependdepends = depend->depends();
			for (QMLItem* dependdepend : dependdepends)
			{
				if (dependdepend == control)
					addError(tq("Circular dependency between control ") + control->name() + tq(" and ") + depend->name());
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
			  [](QMLItem* a, QMLItem* b) { 
					return a->depends().length() < b->depends().length(); 
			});

	for (QMLItem* control : controls)
	{
		_orderedControls.push_back(control);
	}	
}

void AnalysisForm::addListView(QMLListView* listView, const map<QString, QString>& relationMap)
{
	_modelMap[listView->name()] = listView->model();
	_setUpRelatedModels(relationMap);
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
	if (_errorMessagesItem)
	{
		if (!_errorMessages.isEmpty())
		{
			QString text;
			if (_errorMessages.length() == 1)
				text = _errorMessages[0];
			else
			{
				text.append("<ul style=\"margin-bottom:0px\">");
				for (const QString& errorMessage : _errorMessages)
					text.append("<li>").append(errorMessage).append("</li>");
				text.append("</ul>");
			}
			QQmlProperty(_errorMessagesItem, "text").write(QVariant::fromValue(text));
			_errorMessagesItem->setVisible(true);
		}
		else
		{
			QQmlProperty(_errorMessagesItem, "text").write(QVariant::fromValue(QString()));
			_errorMessagesItem->setVisible(false);
		}
	}
}

void AnalysisForm::_setAllAvailableVariablesModel(bool refreshAssigned)
{
	if (_allAvailableVariablesModels.size() == 0)
		return;

	std::vector<std::string> columnNames = _package->getColumnNames();


	for (ListModelTermsAvailable* model : _allAvailableVariablesModels)
	{
		model->initTerms(columnNames);

		if (refreshAssigned)
		{
			QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(model->listView());
			if (qmlAvailableListView)
			{
				const QList<ListModelAssignedInterface*>& assignedModels = qmlAvailableListView->assignedModel();	
				for (ListModelAssignedInterface* modelAssign : assignedModels)
					modelAssign->refresh();
			}
		}
	}

	if(refreshAssigned)
		for(ListModelTermsAvailable * model : _allAvailableVariablesModelsWithSource)
			model->resetTermsFromSourceModels(true);

}

void AnalysisForm::bindTo()
{
	if (_options != nullptr)
		unbind();

	const Json::Value& optionsFromJASPFile = _analysis->optionsFromJASPFile();
	_package = _analysis->getDataSetPackage();
	_options = _analysis->options();
	QVector<ListModelAvailableInterface*> availableModelsToBeReset;

	_options->blockSignals(true);
	
	_setAllAvailableVariablesModel();	
	
	for (QMLItem* control : _orderedControls)
	{
		BoundQMLItem* boundControl = dynamic_cast<BoundQMLItem*>(control);
		if (boundControl)
		{
			std::string name = boundControl->name().toStdString();
			Option* option   = _options->get(name);

			if (option && !boundControl->isOptionValid(option))
			{
				option = nullptr;
				addError(tq("Item " + name + " was loaded with a wrong kind of value." + (optionsFromJASPFile != Json::nullValue ? ". Probably the file comes from an older version of JASP." : "")));
			}

			if (!option)
			{
				option = boundControl->createOption();
				if (optionsFromJASPFile != Json::nullValue)
				{
					const Json::Value& optionValue = optionsFromJASPFile[name];
					if (optionValue != Json::nullValue)
					{
						if (!boundControl->isJsonValid(optionValue))
						{
							std::string labelStr;
							QVariant label = boundControl->getItemProperty("label");
							if (!label.isNull())
								labelStr = label.toString().toStdString();
							if (labelStr.empty())
							{
								label = boundControl->getItemProperty("title");
								labelStr = label.toString().toStdString();
							}
							if (labelStr.empty())
								labelStr = name;
							addError(tq("Control " + labelStr + " was loaded with a wrong kind of value. The file probably comes from an older version of JASP.<br>"
										+ "That means that the results currently displayed do not correspond to the options selected.<br>Refreshing the analysis may change the results"));
						}
						else
							option->set(optionValue);
					}
				}
				_options->add(name, option);
			}

			boundControl->bindTo(option);
		}
		else
		{
			QMLListViewTermsAvailable* availableListControl = dynamic_cast<QMLListViewTermsAvailable *>(control);
			if (availableListControl)
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
					if (optionsFromJASPFile != Json::nullValue)
						availableModel->resetTermsFromSourceModels(false);
					else
						availableModelsToBeReset.push_back(availableModel);
				}
			}
		}
	}

	for (ListModelAvailableInterface* availableModel : availableModelsToBeReset)
		availableModel->resetTermsFromSourceModels(true);
	
	_options->blockSignals(false, false);
}

void AnalysisForm::unbind()
{
	if (_options == nullptr)
		return;
	
	for (QMLItem* control : _orderedControls)
	{
		BoundQMLItem* boundControl = dynamic_cast<BoundQMLItem*>(control);
		if (boundControl)
			boundControl->unbind();
	}

	_options = nullptr;
}

void AnalysisForm::addError(const QString &error)
{
	_errorMessages.append(error);
	_setErrorMessages();
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
		_package	= _analysis->getDataSetPackage();

		_parseQML();

		bool isNewAnalysis = _analysis->options()->size() == 0 && _analysis->optionsFromJASPFile().size() == 0;

		bindTo();
		_analysis->resetOptionsFromJASPFile();
		_analysis->initialized(this, isNewAnalysis);
	}
}

void AnalysisForm::dataSetChangedHandler()
{
	if (!_removed)
	{
		_setAllAvailableVariablesModel(true);
		emit dataSetChanged();
	}
}
