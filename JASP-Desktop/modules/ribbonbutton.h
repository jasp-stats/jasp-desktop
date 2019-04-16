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

#ifndef RIBBONBUTTONMODEL_H
#define RIBBONBUTTONMODEL_H

#include <QAbstractListModel>
#include <QStringList>
#include <QDir>

#include "modules/dynamicmodules.h"
#include "modules/analysismenumodel.h"


class RibbonButton : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		enabled			READ enabled			WRITE setEnabled			NOTIFY enabledChanged)
	Q_PROPERTY(bool		requiresDataset	READ requiresDataset	WRITE setRequiresDataset	NOTIFY requiresDatasetChanged)
	Q_PROPERTY(bool		isDynamic		READ isDynamic			WRITE setIsDynamic			NOTIFY isDynamicChanged)
	Q_PROPERTY(bool		isCommon		READ isCommon			WRITE setIsCommon			NOTIFY isCommonChanged)
	Q_PROPERTY(QString	title			READ titleQ				WRITE setTitleQ				NOTIFY titleChanged)
	Q_PROPERTY(QString	moduleName		READ moduleNameQ									NOTIFY moduleNameChanged)
	Q_PROPERTY(QString	moduleTitle		READ titleQ											NOTIFY titleChanged)
	Q_PROPERTY(QString	iconSource		READ iconSource			WRITE setIconSource			NOTIFY iconSourceChanged)
	Q_PROPERTY(QVariant	analysisMenu	READ analysisMenu									NOTIFY analysisMenuChanged)

public:

	RibbonButton(QObject *parent, Json::Value description, bool isCommon);
	RibbonButton(QObject *parent, Modules::DynamicModule * module);

	bool							requiresDataset()											const			{ return _requiresDataset;						}
	bool							isDynamic()													const			{ return _isDynamicModule;						}
	bool							isCommon()													const			{ return _isCommonModule;						}
	std::string						title()														const			{ return _title;								}
	QString							titleQ()													const			{ return QString::fromStdString(_title);		}
	QString							iconSource()												const			{ return _iconSource;							}
	bool							enabled()													const			{ return _enabled;								}
	std::string						moduleName()												const			{ return _moduleName;							}
	QString							moduleNameQ()												const			{ return QString::fromStdString(_moduleName);	}
	void							setMenu(const Modules::AnalysisEntries& ribbonEntries);
	Modules::DynamicModule*			myDynamicModule();
	Modules::AnalysisEntry*			getAnalysis(const std::string& name);
	QVariant						analysisMenu()												const			{ return QVariant::fromValue(_analysisMenuModel); }

public slots:
	void setRequiresDataset(bool requiresDataset);
	void setIsDynamic(bool isDynamicModule);
	void setIsCommon(bool isCommonModule);
	void setTitle(std::string title);
	void setIconSource(QString iconSource);
	void setTitleQ(QString title)									{ setTitle(title.toStdString()); }
	void setEnabled(bool enabled);
	void setModuleName(std::string moduleName);
	void setModuleNameQ(QString moduleName)							{ setModuleName(moduleName.toStdString()); }
	void somePropertyChanged()										{ emit iChanged(this); }
	void setDynamicModules(DynamicModules * dynamicModules)			{ _dynamicModules = dynamicModules; }
	void descriptionReloaded(Modules::DynamicModule * dynMod);

signals:
	void enabledChanged();
	void requiresDatasetChanged();
	void isDynamicChanged();
	void isCommonChanged();
	void titleChanged();
	void moduleNameChanged();
	void iChanged(RibbonButton * me);
	void iconSourceChanged();
	void analysisMenuChanged();


private:
	void bindYourself();

private:
	AnalysisMenuModel*				_analysisMenuModel;
	Modules::AnalysisEntries		_menuEntries;

	bool							_requiresDataset	= true,
									_isDynamicModule	= true,
									_isCommonModule		= false,
									_enabled			= false;
	std::string						_title				= "",
									_moduleName			= "";
	DynamicModules				*	_dynamicModules		= nullptr;
	Modules::DynamicModule		*	_module				= nullptr;
	QString							_iconSource;
};


#endif
