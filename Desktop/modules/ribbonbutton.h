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
#include <QQmlContext>
#include <QDir>

#include "modules/dynamicmodules.h"
#include "modules/menumodel.h"

#include "dirs.h"
#include "log.h"


class RibbonButton : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		enabled			READ enabled			WRITE setEnabled			NOTIFY enabledChanged		)
	Q_PROPERTY(bool		requiresData	READ requiresData		WRITE setRequiresData		NOTIFY requiresDataChanged	)
	Q_PROPERTY(bool		isCommon		READ isCommon			WRITE setIsCommon			NOTIFY isCommonChanged		)
	Q_PROPERTY(QString	title			READ titleQ				WRITE setTitleQ				NOTIFY titleChanged			)
	Q_PROPERTY(QString	name			READ nameQ											NOTIFY moduleNameChanged	)
	Q_PROPERTY(QString	title			READ titleQ											NOTIFY titleChanged			)
	Q_PROPERTY(QString	iconSource		READ iconSource			WRITE setIconSource			NOTIFY iconSourceChanged	)
	Q_PROPERTY(QVariant	menu			READ menu											NOTIFY analysisMenuChanged	)
	Q_PROPERTY(bool		dataLoaded		READ dataLoaded										NOTIFY dataLoadedChanged	)
	Q_PROPERTY(bool		active			READ active											NOTIFY activeChanged		)
	Q_PROPERTY(QString	toolTip			READ toolTip			WRITE setToolTip			NOTIFY toolTipChanged		)
	Q_PROPERTY(bool		special			READ isSpecial										NOTIFY isSpecialChanged		)
	Q_PROPERTY(bool		ready			READ ready				WRITE setReady				NOTIFY readyChanged			)
	Q_PROPERTY(bool		error			READ error				WRITE setError				NOTIFY errorChanged			)
	Q_PROPERTY(bool		remember		READ remember			WRITE setRemember			NOTIFY rememberChanged		)

public:

	RibbonButton(QObject *parent, Modules::DynamicModule * module);
	RibbonButton(QObject *parent, std::string name,	std::string title, std::string icon, bool requiresData, std::function<void()> justThisFunction, std::string toolTip = "", bool enabled = true, bool remember = false);
	RibbonButton(QObject *parent, std::string name,	std::string title, std::string icon, Modules::AnalysisEntries * funcEntries, bool enabled = true, bool remember = false);
	~RibbonButton() {}


	bool							requiresData()												const			{ return _requiresData;										}
	bool							isCommon()													const			{ return _isCommonModule;									}
	std::string						title()														const			{ return _title;											}
	QString							titleQ()													const			{ return QString::fromStdString(_title);					}
	QString							iconSource()												const			{ return _iconSource;										}
	bool							enabled()													const			{ return _enabled;											}
	std::string						name()														const			{ return _name;												}
	QString							nameQ()														const			{ return QString::fromStdString(_name);						}
	Modules::DynamicModule*			dynamicModule();
	Modules::AnalysisEntry*			getEntry(const std::string& name);
	QVariant						menu()														const			{ return QVariant::fromValue(_menuModel);					}
	std::vector<std::string>		getAllEntries()												const;
	bool							dataLoaded()												const			{ return Modules::DynamicModules::dynMods() &&  Modules::DynamicModules::dynMods()->dataLoaded();	}
	bool							active()													const			{ return _enabled && (!requiresData() || dataLoaded());		}
	QString							toolTip()													const			{ return _toolTip;											}
	bool							isBundled()													const			{ return _module && _module->isBundled();					}
	QString							version()													const			{ return !_module ? "?" : _module->versionQ();				}
	bool							ready()														const			{ return _ready;											}
	bool							error()														const			{ return _error;											}
	bool							remember()													const			{ return _remember;											}

	static QString					getJsonDescriptionFilename();	

public slots:
	void setDynamicModule(Modules::DynamicModule * module);
	void setRequiresData(bool requiresData);
	void setIsCommon(bool isCommonModule);
	void setTitle(std::string title);
	void setTitleQ(QString title)									{ setTitle(title.toStdString()); }
	void setIconSource(QString iconSource);
	void setEnabled(bool enabled);
	void setModuleName(std::string moduleName);
	void setModuleNameQ(QString moduleName)							{ setModuleName(moduleName.toStdString()); }
	void somePropertyChanged()										{ emit iChanged(this); }
	void setToolTip(QString toolTip);
	void setReady(bool ready);
	void setError(bool error);
	void setRemember(bool remember);

	bool isSpecial()			const	{ return _special; }
	void runSpecial(QString func);;
	void reloadDynamicModule(Modules::DynamicModule * dynMod);


signals:
	void enabledChanged();
	void requiresDataChanged();
	void isCommonChanged();
	void titleChanged();
	void moduleNameChanged();
	void iChanged(RibbonButton * me);
	void iconSourceChanged();
	void dataLoadedChanged();
	void activeChanged();
	void toolTipChanged(QString toolTip);
	void analysisMenuChanged();
	void isSpecialChanged(); //This wont be called it is just here to keep qml from complaining
	void readyChanged(bool ready);
	void errorChanged(bool error);

	void rememberChanged(bool remember);

private:
	void bindYourself();


private:
	MenuModel*						_menuModel			= nullptr;
	bool							_requiresData		= true,
									_isDynamicModule	= true,
									_isCommonModule		= false,
									_enabled			= false,
									_ready				= false,
									_error				= false,
									_remember			= true,
									_special			= false;
	std::string						_title				= "",
									_name				= "";
	Modules::DynamicModule		*	_module				= nullptr;
	QString							_iconSource,
									_toolTip;
	std::function<void()>			_specialButtonFunc	= nullptr;
};


#endif
