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

#include "analysisqmlform.h"
#include <QQuickWidget>
#include <QQmlEngine>
#include <QQmlProperty>
#include <QQmlContext>
#include <QMessageBox>
#include <QDebug>

#include "widgets/boundqmlcheckbox.h"
#include "widgets/boundqmltextinput.h"
#include "widgets/boundqmlradiobuttons.h"
#include "widgets/boundqmllistviewpairs.h"
#include "widgets/boundqmllistviewanova.h"
#include "widgets/boundqmllistviewvariables.h"
#include "widgets/boundqmltableview.h"

#include "widgets/listmodeltermsavailable.h"

#include "utils.h"
#include "dirs.h"
#include "settings.h"

using namespace std;

AnalysisQMLForm::AnalysisQMLForm(QWidget *parent, Analysis* analysis) :
    AnalysisForm("AnalysisQMLForm", parent),
	_quickWidget(new QQuickWidget(this)),
	_analysis(analysis)
{
	bool debug = false;
	_allAvailableVariablesModel = NULL;
	_errorMessagesItem = NULL;
	_quickWidget->engine()->addImportPath(":/QMLTheme");
	_quickWidget->setResizeMode(QQuickWidget::SizeRootObjectToView);
	connect(_quickWidget, SIGNAL(statusChanged(QQuickWidget::Status)), this, SLOT(statusChangedWidgetHandler(QQuickWidget::Status)));
	connect(_quickWidget, SIGNAL(sceneGraphError(QQuickWindow::SceneGraphError, QString)), this, SLOT(sceneGraphErrorHandler(QQuickWindow::SceneGraphError, QString)));
#ifdef QT_DEBUG
	debug = true;
	_quickWidget->engine()->clearComponentCache();
#endif
	_quickWidget->engine()->rootContext()->setContextProperty("DEBUG_MODE", debug);
	QString pathToQMLFile = _getAnalysisQMLPath();
	if (!pathToQMLFile.isEmpty())
	{
		_quickWidget->setSource(QUrl(pathToQMLFile));
		_parseQML();
	}
}

QString AnalysisQMLForm::_getAnalysisQMLPath()
{
	const string& module = _analysis->module();
	const string& name = _analysis->name();
	QString path = QString::fromStdString(Dirs::QMLAnalysesDir() + "/" + module + "/"  + name + ".qml");
	if (name == "AnalysisTest")
	{
		QString testAnalyseQMLName = Settings::value(Settings::TEST_ANALYSIS_QML).toString();
		if (!testAnalyseQMLName.isEmpty())
		{
			if (!QFile::exists(testAnalyseQMLName))
			{
				QMessageBox::warning(this, "Error", "Test QML file does not exist: " + testAnalyseQMLName);
				path.clear();
			}
			else
			{
				if (!_QMLwatcher.files().contains(testAnalyseQMLName)) 
				{
					_QMLwatcher.addPath(testAnalyseQMLName);
					connect(&_QMLwatcher, SIGNAL(fileChanged(const QString&)), this, SLOT(QMLFileModifiedHandler(const QString&)));				
				}
				path = QString("file:") + testAnalyseQMLName;
				
				QString testRAnalyseRName = Settings::value(Settings::TEST_ANALYSIS_R).toString();
				if (!testRAnalyseRName.isEmpty())
				{
					if (!QFile::exists(testRAnalyseRName))
					{
						QMessageBox::warning(this, "Error", "Test R file does not exist: " + testRAnalyseRName);
					}
					else
					{
						_analysis->setRFile(testRAnalyseRName.toStdString());
						if (!_Rwatcher.files().contains(testRAnalyseRName))
						{
							_Rwatcher.addPath(testRAnalyseRName);
							connect(&_Rwatcher, SIGNAL(fileChanged(const QString&)), this, SLOT(RFileModifiedHandler(const QString&)));			
							
						}
					}
				}
			}
		}
	}

	return path;
}

void AnalysisQMLForm::_parseQML()
{	
	QQuickItem *root = _quickWidget->rootObject();
	bool usesJaspResults = QQmlProperty(root, "usesJaspResults").read().toBool();
	_analysis->setUsesDataResults(usesJaspResults);
	
	
	map<QString, QString> dropKeyMap;
	QList<QString> controls;
		
	for (QQuickItem* item : root->findChildren<QQuickItem *>())
	{
		if (item->objectName() == "errorMessagesBox")
		{
			_errorMessagesItem = item;
			continue;			
		}
		
		QString controlType = QQmlProperty(item, "controlType").read().toString();			
		if (controlType.isEmpty())
			continue;
		
		bool isBound = QQmlProperty(item, "isBound").read().toBool();
		if (!isBound)
			continue;
		
#ifndef QT_DEBUG
		bool isDebug = QQmlProperty(item, "debug").read().toBool();
		if (isDebug)
			continue;
#endif
		
		QString controlName = QQmlProperty(item, "name").read().toString();
		if (controlName.isEmpty())
		{
			_errorMessages.append(QString::fromLatin1("A control ") + controlType + QString::fromLatin1(" has no name"));
			continue;
		}
		if (controls.contains(controlName))
		{
			_errorMessages.append(QString::fromLatin1("2 controls have the same name: ") + controlName);
			continue;
		}
		
		controls.append(controlName);
		
		BoundQMLItem *boundQMLItem = NULL;
		if (controlType == "CheckBox" || controlType == "Switch")
			boundQMLItem = new BoundQMLCheckBox(item, this);
		else if (controlType == "TextField")
			boundQMLItem = new BoundQMLTextInput(item, this);
		else if (controlType == "ButtonGroup")
			boundQMLItem = new BoundQMLRadioButtons(item, this);
		else if (controlType == "TableView")
			boundQMLItem = new BoundQMLTableView(item, this);
		else if (controlType == "ListView")
		{			
			QString listViewType = QQmlProperty(item, "listViewType").read().toString();
			BoundQMLListView *boundQMLListView = NULL;
			if (listViewType == "assignedVariables")
				boundQMLListView = new BoundQMLListViewVariables(item, this);
			else if (listViewType == "assignedPairs")
				boundQMLListView = new BoundQMLListViewPairs(item, this);
			else if (listViewType == "assignedAnova")
				boundQMLListView = new BoundQMLListViewAnova(item, this);
			else if (listViewType == "availableVariables")
			{
				QString syncModels = QQmlProperty(item, "syncModels").read().toString();
				if (syncModels == "_JASPAllVariables")
				{
					_allAvailableVariablesModel = new ListModelTermsAvailable(this, item);
					_modelMap[controlName] = _allAvailableVariablesModel;
				}
				else
				{					
					ListModelTermsAvailable* availableVariablesModel = new ListModelTermsAvailable(this, item);
					_modelMap[controlName] = availableVariablesModel;
					_availableVariablesModels.push_back(availableVariablesModel);
				}
			}
			else
				_errorMessages.append(QString::fromLatin1("Unknown listViewType: ") + listViewType + QString::fromLatin1(". Cannot set a model to the VariablesList"));
			
			if (boundQMLListView)
			{
				boundQMLItem = boundQMLListView;
				ListModelAssigned* listViewAssigned = boundQMLListView->targetModel();
				_modelMap[controlName] = listViewAssigned;
			}			
						
			QString dropKey;
			QList<QVariant> dropKeyList = QQmlProperty(item, "dropKeys").read().toList();
			if (dropKeyList.isEmpty())
				dropKey = QQmlProperty(item, "dropKeys").read().toString();
			else
				dropKey = dropKeyList[0].toString(); // The first key gives the default drop item.
			
			if (!dropKey.isEmpty())
				dropKeyMap[controlName] = dropKey;
			else
				_errorMessages.append(QString::fromLatin1("No drop key found for ") + controlName);			
			
		}
		else
			_errorMessages.append(QString::fromLatin1("Unknown control type: ") + controlType);
		
		if (boundQMLItem)
			_items.push_back(boundQMLItem);
	}
	
	for (auto const& pair : dropKeyMap) {
		ListModel* source = _modelMap[pair.first];
		ListModel* target = _modelMap[pair.second];
		if (source && target)
			_relatedModel[source->getItem()] = target;
		else
		{
			if (!source)
				_errorMessages.append(QString::fromLatin1("Cannot find a ListView for ") + pair.first);
			else
				_errorMessages.append(QString::fromLatin1("Cannot find a ListView for ") + pair.second);
		}
	}
	
	for (BoundQMLItem* item : _items)
		item->setUp();
	
	if (!_errorMessagesItem)
		qDebug() << "No errorMessages Item found!!!";
	
	_setErrorMessages();
}

void AnalysisQMLForm::_setErrorMessages()
{
	if (!_errorMessages.isEmpty() && _errorMessagesItem)
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
}

AnalysisQMLForm::~AnalysisQMLForm()
{
}

void AnalysisQMLForm::_setAllAvailableVariablesModel()
{
	if (_allAvailableVariablesModel != NULL)
	{
		vector<string> columnNames;
	
		if (_dataSet != NULL)
		{
			for (Column &column: _dataSet->columns())
				columnNames.push_back(column.name());
		}
	
		_allAvailableVariablesModel->initTerms(columnNames);
	}
	
}

void AnalysisQMLForm::bindTo(Options *options, DataSet *dataSet)
{
	if (_options != NULL)
		unbind();

	_dataSet = dataSet;
	_options = options;
	
	_setAllAvailableVariablesModel();	
	
	for (BoundQMLItem* item : _items)
	{
		Option* option = options->get(item->name().toStdString());
		if (option)
		{
			item->bindTo(option);
			item->illegalChanged.connect(boost::bind(&AnalysisForm::illegalValueHandler, this, _1));
		}
		else
		{
			qDebug() << "Cound not find option " << item->name();
		}
	}
	
	if (_options != NULL)
	{
		_options->blockSignals(true);
		for (ListModelTermsAvailable* availableModel : _availableVariablesModels)
		{
			// The availableModel are not bound, but they have to be updated when the form is initialized.
			availableModel->resetTermsFromSyncModels();
		}
		_options->blockSignals(false);
	}
	
	updateIllegalStatus();
}

void AnalysisQMLForm::unbind()
{
	_bounds.clear();
	updateIllegalStatus();

	if (_options == NULL)
		return;
	
	for (BoundQMLItem* item : _items)
		item->unbind();

	_options = NULL;
}


QWidget *AnalysisQMLForm::getWidget()
{
	return _quickWidget;
}

void AnalysisQMLForm::addError(const QString &error)
{
	_errorMessages.append(error);
	_setErrorMessages();
}

ListModel *AnalysisQMLForm::getRelatedModel(QQuickItem *model)
{
	return _relatedModel[model];
}

ListModel *AnalysisQMLForm::getModel(const QString &model)
{
	return _modelMap[model];
}

Options *AnalysisQMLForm::getAnalysisOptions()
{
	return _analysis->options();
}

void AnalysisQMLForm::sceneGraphErrorHandler(QQuickWindow::SceneGraphError error, QString message)
{
	QMessageBox::warning(this, "Error", "Error when painting analysis form: " + message);
}

void AnalysisQMLForm::statusChangedWidgetHandler(QQuickWidget::Status status)
{
	if (status == QQuickWidget::Error)
	{
		QQuickWidget* widget = qobject_cast<QQuickWidget*>(sender());
		QString message;
		for (QQmlError error : widget->errors())
		{
			if (!message.isEmpty())
				message += '\n';
			message += error.toString();
		}
		QMessageBox::warning(this, "Error", "Error when loading analysis form: \n" + message);
	}
}

void AnalysisQMLForm::QMLFileModifiedHandler(QString path)
{
	qDebug() << "Test QML file modified";
	_items.empty();
	_relatedModel.empty();
	_availableVariablesModels.empty();
	_modelMap.empty();
	
	_quickWidget->engine()->clearComponentCache();
	emit formChanged(_analysis);
}

void AnalysisQMLForm::RFileModifiedHandler(QString path)
{
	qDebug() << "Test R file modified";
	
}
