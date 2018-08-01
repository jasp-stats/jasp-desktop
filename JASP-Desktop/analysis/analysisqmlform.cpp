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
#include "utilities/settings.h"

using namespace std;

AnalysisQMLForm::AnalysisQMLForm(QWidget *parent, Analysis* analysis)
	:  AnalysisForm("AnalysisQMLForm", parent), _quickWidget(new QQuickWidget(this)), _analysis(analysis), _allAvailableVariablesModel(NULL),	_errorMessagesItem(NULL)
{
	_quickWidget->engine()->addImportPath(":/QMLTheme");
	_quickWidget->setResizeMode(QQuickWidget::SizeRootObjectToView);

	connect(_quickWidget,	&QQuickWidget::statusChanged,	this,	&AnalysisQMLForm::statusChangedWidgetHandler);
	connect(_quickWidget,	&QQuickWidget::sceneGraphError,	this,	&AnalysisQMLForm::sceneGraphErrorHandler);

	bool debug = false;
#ifdef JASP_DEBUG
	debug = true;
	_quickWidget->engine()->clearComponentCache();
#endif
	_quickWidget->engine()->rootContext()->setContextProperty("DEBUG_MODE", debug);

	QString pathToQMLFile = _getAnalysisQMLPath();

	if (!pathToQMLFile.isEmpty())
	{
#ifdef JASP_DEBUG
		std::cout << "Succesfully loaded QML file path for analysis: " << pathToQMLFile.toStdString() << ", now being set as source of qquickwidget, actually loaded and parsed." << std::endl;
#endif
		_quickWidget->setSource(QUrl(pathToQMLFile));
		_parseQML();
	}
}

QString AnalysisQMLForm::_getAnalysisQMLPath()
{
	QString path			= QString::fromStdString(_analysis->qmlFormPath());

	if (_analysis->name() == "AnalysisTest")
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
					connect(&_QMLwatcher, &QFileSystemWatcher::fileChanged, this, &AnalysisQMLForm::QMLFileModifiedHandler);
				}
				path = QString("file:") + testAnalyseQMLName;
				
				QString testRAnalyseRName = Settings::value(Settings::TEST_ANALYSIS_R).toString();
				if (!testRAnalyseRName.isEmpty())
				{
					if (!QFile::exists(testRAnalyseRName))
						QMessageBox::warning(this, "Error", "Test R file does not exist: " + testRAnalyseRName);
					else
					{
						_analysis->setRFile(testRAnalyseRName.toStdString());
						if (!_Rwatcher.files().contains(testRAnalyseRName))
						{
							_Rwatcher.addPath(testRAnalyseRName);
							connect(&_Rwatcher, &QFileSystemWatcher::fileChanged, this, &AnalysisQMLForm::RFileModifiedHandler);
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

	_analysis->setUsesJaspResults(QQmlProperty(root, "usesJaspResults").read().toBool());
	
	map<QString, QString>	dropKeyMap;
	QList<QString>			controls;
		
	for (QQuickItem* item : root->findChildren<QQuickItem *>())
	{
		if (item->objectName() == "errorMessagesBox")
		{
			_errorMessagesItem = item;
			continue;			
		}
		
		QString controlTypeStr = QQmlProperty(item, "controlType").read().toString();
		if (controlTypeStr.isEmpty())
			continue;
		
		if (! QQmlProperty(item, "isBound").read().toBool())
			continue;
		
#ifndef QT_DEBUG
		bool isDebug = QQmlProperty(item, "debug").read().toBool();
		if (isDebug)
			continue;
#endif
		
		QString controlName = QQmlProperty(item, "name").read().toString();
		if (controlName.isEmpty())
		{
			_errorMessages.append(QString::fromLatin1("A control ") + controlTypeStr + QString::fromLatin1(" has no name"));
			continue;
		}

		if (controls.contains(controlName))
		{
			_errorMessages.append(QString::fromLatin1("2 controls have the same name: ") + controlName);
			continue;
		}
		
		controls.append(controlName);
		
		BoundQMLItem *boundQMLItem = NULL;
		qmlControlType controlType = qmlControlTypeFromQString(controlTypeStr);

		switch(controlType)
		{
		case qmlControlType::CheckBox:		//fallthrough:
		case qmlControlType::Switch:		boundQMLItem = new BoundQMLCheckBox(item,		this);	break;
		case qmlControlType::TextField:		boundQMLItem = new BoundQMLTextInput(item,		this);	break;
		case qmlControlType::ButtonGroup:	boundQMLItem = new BoundQMLRadioButtons(item,	this);	break;
		case qmlControlType::TableView:		boundQMLItem = new BoundQMLTableView(item,		this);	break;

		case qmlControlType::ListView:
		{			
			QString			listViewTypeStr = QQmlProperty(item, "listViewType").read().toString();
			qmlListViewType	listViewType;

			try							{ listViewType	= qmlListViewTypeFromQString(listViewTypeStr);	}
			catch(std::out_of_range)	{ _errorMessages.append(QString::fromLatin1("Unknown listViewType: ") + listViewType + QString::fromLatin1(". Cannot set a model to the VariablesList")); }

			BoundQMLListView *boundQMLListView = NULL;
			switch(listViewType)
			{
			case qmlListViewType::assignedVariables:	boundQMLListView = new BoundQMLListViewVariables(item,	this);	break;
			case qmlListViewType::assignedPairs:		boundQMLListView = new BoundQMLListViewPairs(item,		this);	break;
			case qmlListViewType::assignedAnova:		boundQMLListView = new BoundQMLListViewAnova(item,		this);	break;
			case qmlListViewType::availableVariables:
			{
				auto* availableVariablesModel	= new ListModelTermsAvailable(this, item);
				_modelMap[controlName]			= availableVariablesModel;

				if (QQmlProperty(item, "syncModels").read().toString() == "_JASPAllVariables")
					_allAvailableVariablesModel	= availableVariablesModel;
				else
					_availableVariablesModels.push_back(availableVariablesModel);
				break;
			}
			default:
				_errorMessages.append(QString::fromLatin1("Unused (in AnalysisQMLForm::_parseQML) listViewType: ") + qmlListViewTypeToQString(listViewType) + QString::fromLatin1(". Cannot set a model to the VariablesList"));
				break;
			}

			
			if (boundQMLListView)
			{
				boundQMLItem			= boundQMLListView;
				auto* listViewAssigned	= boundQMLListView->targetModel();
				_modelMap[controlName]	= listViewAssigned;
			}			
						
			QList<QVariant> dropKeyList = QQmlProperty(item, "dropKeys").read().toList();
			QString dropKey				= dropKeyList.isEmpty() ? QQmlProperty(item, "dropKeys").read().toString() : dropKeyList[0].toString(); // The first key gives the default drop item.
			
			if (!dropKey.isEmpty())
				dropKeyMap[controlName] = dropKey;
			else
				_errorMessages.append(QString::fromLatin1("No drop key found for ") + controlName);

			break;
		}
		}
		
		if (boundQMLItem)
			_items.push_back(boundQMLItem);
	}
	
	for (auto const& pair : dropKeyMap)
	{
		ListModel* source = _modelMap[pair.first];
		ListModel* target = _modelMap[pair.second];

		if (source && target)
			_relatedModel[source->getItem()] = target;
		else
			_errorMessages.append(QString::fromLatin1("Cannot find a ListView for ") + (!source ? pair.first : pair.second));
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

void AnalysisQMLForm::_setAllAvailableVariablesModel()
{
	if (_allAvailableVariablesModel != NULL)
	{
		vector<string> columnNames;
	
		if (_dataSet != NULL)
			for (Column &column: _dataSet->columns())
				columnNames.push_back(column.name());
	
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
			qDebug() << "Cound not find option " << item->name();
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

void AnalysisQMLForm::addError(const QString &error)
{
	_errorMessages.append(error);
	_setErrorMessages();
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
